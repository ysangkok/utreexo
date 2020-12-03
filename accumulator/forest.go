package accumulator

import (
	"encoding/binary"
	"fmt"
	"os"
	"sort"
	"time"
)

// A FullForest is the entire accumulator of the UTXO set. This is
// what the bridge node stores.  Everything is always full.

/*
The forest is structured in the space of a tree numbered from the bottom left,
taking up the space of a perfect tree that can contain the whole forest.
This means that in most cases there will be null nodes in the tree.
That's OK; it helps reduce renumbering nodes and makes it easier to think about
addressing.  It also might work well for on-disk serialization.
There might be a better / optimal way to do this but it seems OK for now.
*/

// Forest is the entire accumulator of the UTXO set as either a:
// 1) slice if the forest is stored in memory.
// 2) single file if the forest is stored in disk.
// A leaf represents a UTXO with additional data for verification.
// This leaf is numbered from bottom left to right.
// Example of a forest with 4 numLeaves:
//
//	06
//	|------\
//	04......05
//	|---\...|---\
//	00..01..02..03
//
// 04 is the concatenation and the hash of 00 and 01. 06 is the root
// This tree would have a row of 2.
type Forest struct {
	NumLeaves uint64 // number of leaves in the forest (bottom row)

	// Rows in the forest. (forest height) NON INTUITIVE!
	// When there is only 1 tree in the forest, it is equal to the Rows of
	// that tree (2**h nodes).  If there are multiple trees, Rows will
	// be 1 more than the tallest tree in the forest.
	// While you could just run treeRows(numLeaves), and pollard does just this,
	// here it incurs the cost of a reMap when you cross a power of 2 boundary.
	// So right now it reMaps on the way up, but NOT on the way down, so the
	// Rows can sometimes be higher than it would be as treeRows(numLeaves)
	// A little weird; could remove this, but likely would have a performance
	// penalty if the set dances right above / below a power of 2 leaves.
	Rows uint8

	// "Data" (not the best name but) is an interface to storing the forest
	// hashes.  There's ram based and disk based for now, maybe if one
	// is clearly better can go back to non-interface.
	Data ForestData
	// moving to slice based forest.  more efficient, can be moved to
	// an on-disk file more easily (the subtree stuff should be changed
	// at that point to do runs of i/o).  Not sure about "deleting" as it
	// might not be needed at all with a slice.

	PositionMap map[MiniHash]uint64 // map from hashes to positions.
	// Inverse of forestMap for leaves.

	/*
	 * below are just for testing / benchmarking
	 */

	// HistoricHashes represents how many hashes this forest has computed
	//
	// Meant for testing / benchmarking
	HistoricHashes uint64

	// TimeRem represents how long Remove() function took
	//
	// Meant for testing / benchmarking
	TimeRem time.Duration

	// TimeMST represents how long the moveSubTree() function took
	//
	// Meant for testing / benchmarking
	TimeMST time.Duration

	// TimeInHash represents how long the hash operations (reHash) took
	//
	// Meant for testing / benchmarking
	TimeInHash time.Duration

	// TimeInProve represents how long the Prove operations took
	//
	// Meant for testing / benchmarking
	TimeInProve time.Duration

	// TimeInVerify represents how long the verify operations took
	//
	// Meant for testing / benchmarking
	TimeInVerify time.Duration
}

// NewForest : use ram if not given a file
func NewForest(forestFile *os.File, cached bool,
	cowPath string, cowMaxCache int) *Forest {

	f := new(Forest)
	f.NumLeaves = 0
	f.Rows = 0

	if forestFile == nil {
		if cowPath == "" {
			// for in-ram
			f.Data = new(RamForestData)
		} else {
			// Init cowForest
			d, err := initialize(cowPath, cowMaxCache)
			if err != nil {
				panic(err)
			}
			f.Data = d
		}

	} else {
		// forest on disk or cached
		if cached {
			d := new(cacheForestData)
			d.file = forestFile
			d.cache = newDiskForestCache(20)
			f.Data = d
		} else {
			// for on-disk
			d := new(diskForestData)
			d.file = forestFile
			f.Data = d
		}
	}

	f.Data.resize((2 << f.Rows) - 1)
	f.PositionMap = make(map[MiniHash]uint64)
	return f
}

// TODO remove, only here for testing
func (f *Forest) ReconstructStats() (uint64, uint8) {
	return f.NumLeaves, f.Rows
}

const sibSwap = false
const bridgeVerbose = false

// empty is needed for detection (to find errors) but I'm not sure it's needed
// for deletion.  I think you can just leave garbage around, as it'll either
// get immediately overwritten, or it'll be out to the right, beyond the edge
// of the forest
var empty [32]byte

// TODO forest.removev4 and pollard.rem2 are VERY similar.  It seems like
// whether it's forest or pollard, most of the complicated stuff is the same.
// so maybe they can both satisfy an interface.  In the case of remove, the only
// specific calls are HnFromPos and swapNodes
//
//

// rnew -- emove v4 with swapHashRange
func (f *Forest) removev4(dels []uint64) error {
	nextNumLeaves := f.NumLeaves - uint64(len(dels))
	// check that all dels are there
	for _, dpos := range dels {
		if dpos > f.NumLeaves {
			return fmt.Errorf(
				"Trying to delete leaf at %d, beyond max %d", dpos, f.NumLeaves)
		}
	}
	var hashDirt []uint64
	// fmt.Printf("call rem2 nl %d rem %v\n", f.numLeaves, dels)
	swapRows := remTrans2(dels, f.NumLeaves, f.Rows)
	// fmt.Printf("got swaps %v\n", swapRows)
	// loop taken from pollard rem2.  maybe pollard and forest can both
	// satisfy the same interface..?  maybe?  that could work...
	// TODO try that ^^^^^^
	for r := uint8(0); r < f.Rows; r++ {
		hashDirt = updateDirt(hashDirt, swapRows[r], f.NumLeaves, f.Rows)
		for _, swap := range swapRows[r] {
			f.SwapNodes(swap, r)
		}
		// do all the hashes at once at the end
		err := f.HashRow(hashDirt)
		if err != nil {
			return err
		}
	}
	f.NumLeaves = nextNumLeaves

	return nil
}

func updateDirt(hashDirt []uint64, swapRow []Arrow, numLeaves uint64, rows uint8) (nextHashDirt []uint64) {
	var prevHash uint64
	hashDirt = dedupeSwapDirt(hashDirt, swapRow)
	for len(swapRow) != 0 || len(hashDirt) != 0 {
		// check if doing dirt. if not dirt, swap.
		// (maybe a little clever here...)
		popSwap, hashDest := makeDestInRow(swapRow, hashDirt, rows)
		if popSwap {
			swapRow = swapRow[1:]
		} else {
			hashDirt = hashDirt[1:]
		}
		if !inForest(hashDest, numLeaves, rows) ||
			hashDest == 0 || // TODO would be great to use nextNumLeaves... but tricky
			hashDest == prevHash { // TODO this doesn't cover everything
			continue
		}
		prevHash = hashDest
		i := sort.Search(len(nextHashDirt), func(i int) bool {
			return nextHashDirt[i] >= hashDest
		})
		if i >= len(nextHashDirt) || nextHashDirt[i] != hashDest {
			// hashDest was not in the list, and i is where
			// it should be inserted
			nextHashDirt = append(nextHashDirt, 0)
			copy(nextHashDirt[i+1:], nextHashDirt[i:])
			nextHashDirt[i] = hashDest
		}
	}
	return nextHashDirt
}

func makeDestInRow(maybeArrow []Arrow, hashDirt []uint64, rows uint8) (bool, uint64) {
	if len(maybeArrow) == 0 {
		// re-descending here which isn't great
		hashDest := parent(hashDirt[0], rows)
		return false, hashDest
	}

	// swapping
	hashDest := parent(maybeArrow[0].To, rows)
	return true, hashDest
}

func (f *Forest) SwapNodes(s Arrow, row uint8) bool {
	if s.From == s.To {
		// these shouldn't happen, and seems like the don't

		fmt.Printf("%s\nmove %d to %d\n", f.ToString(), s.From, s.To)
		panic("got non-moving swap")
	}
	if row == 0 {
		f.Data.swapHash(s.From, s.To)
		f.PositionMap[f.Data.Read(s.To).Mini()] = s.To
		f.PositionMap[f.Data.Read(s.From).Mini()] = s.From
		return true
	}
	// fmt.Printf("swapnodes %v\n", s)
	a := childMany(s.From, row, f.Rows)
	b := childMany(s.To, row, f.Rows)
	rowa := detectRow(a, f.Rows)
	rowb := detectRow(b, f.Rows)
	if rowa != rowb {
		panic("row mismatch")
	}
	if a == b {
		//fmt.Println("skipping this", a, s.From, s.To)
		// TODO this should probably actually panic
		panic(fmt.Sprintf("how can this happen %v %v %v", s, row, f.Rows))
		return false
	}
	run := uint64(1 << row)

	// happens before the actual swap, so swapping a and b
	for i := uint64(0); i < run; i++ {
		f.PositionMap[f.Data.Read(a+i).Mini()] = b + i
		f.PositionMap[f.Data.Read(b+i).Mini()] = a + i
	}

	// start at the bottom and go to the top
	for r := uint8(0); r <= row; r++ {
		// fmt.Printf("shr %d %d %d\n", a, b, run)
		f.Data.swapHashRange(a, b, run)
		a = parent(a, f.Rows)
		b = parent(b, f.Rows)
		run >>= 1
	}
	return true
}

// reHash hashes new data in the forest based on dirty positions.
// right now it seems "dirty" means the node itself moved, not that the
// parent has changed children.
// TODO: switch the meaning of "dirt" to mean parents with changed children;
// this will probably make it a lot simpler.
func (f *Forest) reHash(dirt []uint64) error {
	if f.Rows == 0 || len(dirt) == 0 { // nothing to hash
		return nil
	}
	rootPositions, rootRows := getRootsReverse(f.NumLeaves, f.Rows)

	dirty2d := make([][]uint64, f.Rows)
	r := uint8(0)
	dirtyRemaining := 0
	for _, pos := range dirt {
		if pos > f.NumLeaves {
			return fmt.Errorf("Dirt %d exceeds numleaves %d", pos, f.NumLeaves)
		}
		dRow := detectRow(pos, f.Rows)
		// increase rows if needed
		for r < dRow {
			r++
		}
		if r > f.Rows {
			return fmt.Errorf("position %d at row %d but forest only %d high",
				pos, r, f.Rows)
		}
		// if bridgeVerbose {
		// fmt.Printf("h %d\n", h)
		// }
		dirty2d[r] = append(dirty2d[r], pos)
		dirtyRemaining++
	}

	// this is basically the same as VerifyBlockProof.  Could maybe split
	// it to a separate function to reduce redundant code..?
	// nah but pretty different because the dirtyMap has stuff that appears
	// halfway up...

	var currentRow, nextRow []uint64

	// floor by floor
	for r = uint8(0); r < f.Rows; r++ {
		if bridgeVerbose {
			fmt.Printf("dirty %v\ncurrentRow %v\n", dirty2d[r], currentRow)
		}

		// merge nextRow and the dirtySlice.  They're both sorted so this
		// should be quick.  Seems like a CS class kind of algo but who knows.
		// Should be O(n) anyway.

		currentRow = mergeSortedSlices(currentRow, dirty2d[r])
		dirtyRemaining -= len(dirty2d[r])
		if dirtyRemaining == 0 && len(currentRow) == 0 {
			// done hashing early
			break
		}

		for i, pos := range currentRow {
			// skip if next is sibling
			if i+1 < len(currentRow) && currentRow[i]|1 == currentRow[i+1] {
				continue
			}
			if len(rootPositions) == 0 {
				return fmt.Errorf(
					"currentRow %v no roots remaining, this shouldn't happen",
					currentRow)
			}
			// also skip if this is a root
			if pos == rootPositions[0] {
				continue
			}

			right := pos | 1
			left := right ^ 1
			parpos := parent(left, f.Rows)

			//				fmt.Printf("bridge hash %d %04x, %d %04x -> %d\n",
			//					left, leftHash[:4], right, rightHash[:4], parpos)
			if f.Data.Read(left) == empty || f.Data.Read(right) == empty {
				f.Data.write(parpos, empty)
			} else {
				par := parentHash(f.Data.Read(left), f.Data.Read(right))
				f.HistoricHashes++
				f.Data.write(parpos, par)
			}
			nextRow = append(nextRow, parpos)
		}
		if rootRows[0] == r {
			rootPositions = rootPositions[1:]
			rootRows = rootRows[1:]
		}
		currentRow = nextRow
		nextRow = []uint64{}
	}

	return nil
}

// cleanup removes extraneous hashes from the forest.  Currently only the bottom
// Probably don't need this at all, if everything else is working.
func (f *Forest) cleanup(overshoot uint64) {
	for p := f.NumLeaves; p < f.NumLeaves+overshoot; p++ {
		delete(f.PositionMap, f.Data.Read(p).Mini()) // clear position map
		// TODO ^^^^ that probably does nothing. or at least should...
		// f.data.write(p, empty) // clear forest
	}
}

// Add adds leaves to the forest.  This is the easy part.
func (f *Forest) Add(adds []Leaf) {
	f.addv2(adds)
}

// Add adds leaves to the forest.  This is the easy part.
func (f *Forest) addv2(adds []Leaf) {

	for _, add := range adds {
		// fmt.Printf("adding %x pos %d\n", add.Hash[:4], f.numLeaves)
		f.PositionMap[add.Mini()] = f.NumLeaves

		rootPositions, _ := getRootsReverse(f.NumLeaves, f.Rows)
		pos := f.NumLeaves
		n := add.Hash
		f.Data.write(pos, n)
		for h := uint8(0); (f.NumLeaves>>h)&1 == 1; h++ {
			// grab, pop, swap, hash, new
			root := f.Data.Read(rootPositions[h]) // grab
			//			fmt.Printf("grabbed %x from %d\n", root[:12], roots[h])
			n = parentHash(root, n)   // hash
			pos = parent(pos, f.Rows) // rise
			f.Data.write(pos, n)      // write
			//			fmt.Printf("wrote %x to %d\n", n[:4], pos)
		}
		f.NumLeaves++
	}
}

// PrepareInsertion remaps the forest until large enough
func (f *Forest) PrepareInsertion(delta uint64) error {
	// remap to expand the forest if needed
	for f.NumLeaves+delta > uint64(1<<f.Rows) {
		// fmt.Printf("current cap %d need %d\n",
		// 1<<f.Rows, f.NumLeaves+delta)
		err := f.reMap(f.Rows + 1)
		if err != nil {
			return err
		}
	}
	return nil
}

// Modify changes the forest, adding and deleting leaves and updating internal nodes.
// Note that this does not modify in place!  All deletes occur simultaneous with
// adds, which show up on the right.
// Also, the deletes need there to be correct proof data, so you should first call Verify().
func (f *Forest) Modify(adds []Leaf, delsUn []uint64) (*undoBlock, error) {
	numdels, numadds := len(delsUn), len(adds)
	delta := int64(numadds - numdels) // watch 32/64 bit
	if int64(f.NumLeaves)+delta < 0 {
		return nil, fmt.Errorf("can't delete %d leaves, only %d exist",
			len(delsUn), f.NumLeaves)
	}

	// if !checkSortedNoDupes(dels) { // check for sorted deletion slice
	// fmt.Printf("%v\n", dels)
	// return nil, fmt.Errorf("Deletions in incorrect order or duplicated")
	// }
	// TODO for now just sort
	dels := make([]uint64, len(delsUn))
	copy(dels, delsUn)
	sortUint64s(dels)

	for _, a := range adds { // check for empty leaves
		if a.Hash == empty {
			return nil, fmt.Errorf("Can't add empty (all 0s) leaf to accumulator")
		}
	}
	// remap to expand the forest if needed
	for int64(f.NumLeaves)+delta > int64(1<<f.Rows) {
		// fmt.Printf("current cap %d need %d\n",
		// 1<<f.rows, f.numLeaves+delta)
		err := f.reMap(f.Rows + 1)
		if err != nil {
			return nil, err
		}
	}

	// v3 should do the exact same thing as v2 now
	err := f.removev4(dels)
	if err != nil {
		return nil, err
	}
	f.cleanup(uint64(numdels))

	// save the leaves past the edge for undo
	// dels hasn't been mangled by remove up above, right?
	// BuildUndoData takes all the stuff swapped to the right by removev3
	// and saves it in the order it's in, which should make it go back to
	// the right place when it's swapped in reverse
	ub := f.BuildUndoData(uint64(numadds), dels)

	f.addv2(adds)

	// fmt.Printf("done modifying block, added %d\n", len(adds))
	// fmt.Printf("post add %s\n", f.ToString())
	// for m, p := range f.positionMap {
	// 	fmt.Printf("%x @%d\t", m[:4], p)
	// }
	// fmt.Printf("\n")

	return ub, err
}

// reMap changes the rows in the forest
func (f *Forest) reMap(destRows uint8) error {

	if destRows == f.Rows {
		return fmt.Errorf("can't remap %d to %d... it's the same",
			destRows, destRows)
	}

	if destRows > f.Rows+1 || (f.Rows > 0 && destRows < f.Rows-1) {
		return fmt.Errorf("changing by more than 1 not programmed yet")
	}

	// for row reduction
	if destRows < f.Rows {
		return fmt.Errorf("row reduction not implemented")
	}
	// I don't think you ever need to remap down.  It really doesn't
	// matter.  Something to program someday if you feel like it for fun.
	// fmt.Printf("size is %d\n", f.data.size())
	// rows increase
	f.Data.resize((2 << destRows) - 1)
	// fmt.Printf("size is %d\n", f.data.size())
	pos := uint64(1 << destRows) // leftmost position of row 1
	reach := pos >> 1            // how much to next row up
	// start on row 1, row 0 doesn't move
	for h := uint8(1); h < destRows; h++ {
		runLength := reach >> 1
		for x := uint64(0); x < runLength; x++ {
			// ok if source position is non-empty
			ok := f.Data.Size() > (pos>>1)+x &&
				f.Data.Read((pos>>1)+x) != empty
			src := f.Data.Read((pos >> 1) + x)
			if ok {
				f.Data.write(pos+x, src)
			}
		}
		pos += reach
		reach >>= 1
	}

	// zero out (what is now the) right half of the bottom row
	//	copy(t.fs[1<<(t.rows-1):1<<t.rows], make([]Hash, 1<<(t.rows-1)))
	for x := uint64(1 << f.Rows); x < 1<<destRows; x++ {
		// here you may actually need / want to delete?  but numleaves
		// should still ensure that you're not reading over the edge...
		f.Data.write(x, empty)
	}

	f.Rows = destRows
	return nil
}

// sanity checks forest sanity: does numleaves make sense, and are the roots
// populated?
func (f *Forest) sanity() error {

	if f.NumLeaves > 1<<f.Rows {
		return fmt.Errorf("forest has %d leaves but insufficient rows %d",
			f.NumLeaves, f.Rows)
	}
	rootPositions, _ := getRootsReverse(f.NumLeaves, f.Rows)
	for _, t := range rootPositions {
		if f.Data.Read(t) == empty {
			return fmt.Errorf("Forest has %d leaves %d roots, but root @%d is empty",
				f.NumLeaves, len(rootPositions), t)
		}
	}
	if uint64(len(f.PositionMap)) > f.NumLeaves {
		return fmt.Errorf("sanity: positionMap %d leaves but forest %d leaves",
			len(f.PositionMap), f.NumLeaves)
	}

	return nil
}

// PosMapSanity is costly / slow: check that everything in posMap is correct
func (f *Forest) PosMapSanity() error {
	for i := uint64(0); i < f.NumLeaves; i++ {
		if f.PositionMap[f.Data.Read(i).Mini()] != i {
			return fmt.Errorf("positionMap error: map says %x @%d but @%d",
				f.Data.Read(i).Prefix(), f.PositionMap[f.Data.Read(i).Mini()], i)
		}
	}
	return nil
}

// RestoreForest restores the forest on restart. Needed when resuming after exiting.
// miscForestFile is where numLeaves and rows is stored
func RestoreForest(
	miscForestFile *os.File, forestFile *os.File,
	toRAM, cached bool, cow string, cowMaxCache int) (*Forest, error) {

	// start a forest for restore
	f := new(Forest)

	// Restore the numLeaves
	err := binary.Read(miscForestFile, binary.BigEndian, &f.NumLeaves)
	if err != nil {
		return nil, err
	}
	fmt.Println("Forest leaves:", f.NumLeaves)
	// Restore number of rows
	// TODO optimize away "rows" and only save in minimzed form
	// (this requires code to shrink the forest
	binary.Read(miscForestFile, binary.BigEndian, &f.Rows)
	if err != nil {
		return nil, err
	}
	fmt.Println("Forest rows:", f.Rows)

	if cow != "" {
		cowData, err := loadCowForest(cow, cowMaxCache)
		if err != nil {
			return nil, err
		}

		f.Data = cowData
	} else {
		// open the forest file on disk even if we're going to ram
		diskData := new(diskForestData)
		diskData.file = forestFile

		if toRAM {
			// for in-ram
			ramData := new(RamForestData)
			fmt.Printf("%d rows resize to %d\n", f.Rows, (2<<f.Rows - 1))
			ramData.resize((2 << f.Rows) - 1)

			// Can't read all at once!  There's a (secret? at least not well
			// documented) maxRW of 1GB.
			var bytesRead int
			for bytesRead < len(ramData.M) {
				n, err := diskData.file.Read(ramData.M[bytesRead:])
				if err != nil {
					return nil, err
				}
				bytesRead += n
				fmt.Printf("read %d bytes of forest file into ram\n", bytesRead)
			}

			f.Data = ramData

			// for i := uint64(0); i < f.data.size(); i++ {
			// f.data.write(i, diskData.read(i))
			// if i%100000 == 0 && i != 0 {
			// fmt.Printf("read %d nodes from disk\n", i)
			// }
			// }

		} else {
			if cached {
				// on disk, with cache
				cfd := new(cacheForestData)
				cfd.cache = newDiskForestCache(20)
				cfd.file = forestFile
				f.Data = cfd
			} else {
				// on disk, no cache
				f.Data = diskData
			}
			// assume no resize needed
		}
	}

	// Restore positionMap by rebuilding from all leaves
	f.PositionMap = make(map[MiniHash]uint64)
	fmt.Printf("%d leaves for position map\n", f.NumLeaves)
	for i := uint64(0); i < f.NumLeaves; i++ {
		f.PositionMap[f.Data.Read(i).Mini()] = i
		if i%100000 == 0 && i != 0 {
			fmt.Printf("Added %d leaves %x\n", i, f.Data.Read(i).Mini())
		}
	}
	if f.PositionMap == nil {
		return nil, fmt.Errorf("Generated positionMap is nil")
	}

	fmt.Println("Done restoring forest")

	// for cacheForestData the `hashCount` field gets
	// set throught the size() call.
	f.Data.Size()

	return f, nil
}

func (f *Forest) PrintPositionMap() string {
	var s string
	for pos := uint64(0); pos < f.NumLeaves; pos++ {
		l := f.Data.Read(pos).Mini()
		s += fmt.Sprintf("pos %d, leaf %x map to %d\n", pos, l, f.PositionMap[l])
	}

	return s
}

// WriteMiscData writes the numLeaves and rows to miscForestFile
func (f *Forest) WriteMiscData(miscForestFile *os.File) error {
	fmt.Println("numLeaves=", f.NumLeaves)
	fmt.Println("f.rows=", f.Rows)

	err := binary.Write(miscForestFile, binary.BigEndian, f.NumLeaves)
	if err != nil {
		return err
	}

	err = binary.Write(miscForestFile, binary.BigEndian, f.Rows)
	if err != nil {
		return err
	}

	f.Data.close()

	return nil
}

// WriteForestToDisk writes the whole forest to disk
// this only makes sense to do if the forest is in ram.  So it'll return
// an error if it's not a ramForestData
func (f *Forest) WriteForestToDisk(dumpFile *os.File, ram, cow bool) error {

	if ram {
		ramForest, ok := f.Data.(*RamForestData)
		if !ok {
			return fmt.Errorf("WriteForest only possible with ram forest")
		}
		_, err := dumpFile.Seek(0, 0)
		if err != nil {
			return fmt.Errorf("WriteForest seek %s", err.Error())
		}
		_, err = dumpFile.Write(ramForest.M)
		if err != nil {
			return fmt.Errorf("WriteForest write %s", err.Error())
		}
	}

	if cow {
		//fmt.Println("F.DATA.CLOSE ON COW")
		//fmt.Println("TYPE:")
		//fmt.Printf("%T\n", f.data)
		//f.data.close()
	}

	return nil
}

// getRoots returns all the roots of the trees
func (f *Forest) getRoots() []Hash {

	rootPositions, _ := getRootsReverse(f.NumLeaves, f.Rows)
	roots := make([]Hash, len(rootPositions))

	for i, _ := range roots {
		roots[i] = f.Data.Read(rootPositions[i])
	}

	return roots
}

// Stats :
func (f *Forest) Stats() string {

	s := fmt.Sprintf("numleaves: %d hashesever: %d posmap: %d forest: %d\n",
		f.NumLeaves, f.HistoricHashes, len(f.PositionMap), f.Data.Size())

	s += fmt.Sprintf("\thashT: %.2f remT: %.2f (of which MST %.2f) proveT: %.2f",
		f.TimeInHash.Seconds(), f.TimeRem.Seconds(), f.TimeMST.Seconds(),
		f.TimeInProve.Seconds())
	return s
}

// ToString prints out the whole thing.  Only viable for small forests
func (f *Forest) ToString() string {

	fh := f.Rows
	// tree rows should be 6 or less
	if fh > 6 {
		return "forest too big to print "
	}

	output := make([]string, (fh*2)+1)
	var pos uint8
	for h := uint8(0); h <= fh; h++ {
		rowlen := uint8(1 << (fh - h))

		for j := uint8(0); j < rowlen; j++ {
			var valstring string
			ok := f.Data.Size() >= uint64(pos)
			if ok {
				val := f.Data.Read(uint64(pos))
				if val != empty {
					valstring = fmt.Sprintf("%x", val[:2])
				}
			}
			if valstring != "" {
				output[h*2] += fmt.Sprintf("%02d:%s ", pos, valstring)
			} else {
				output[h*2] += "        "
			}
			if h > 0 {
				//				if x%2 == 0 {
				output[(h*2)-1] += "|-------"
				for q := uint8(0); q < ((1<<h)-1)/2; q++ {
					output[(h*2)-1] += "--------"
				}
				output[(h*2)-1] += "\\       "
				for q := uint8(0); q < ((1<<h)-1)/2; q++ {
					output[(h*2)-1] += "        "
				}

				//				}

				for q := uint8(0); q < (1<<h)-1; q++ {
					output[h*2] += "        "
				}

			}
			pos++
		}

	}
	var s string
	for z := len(output) - 1; z >= 0; z-- {
		s += output[z] + "\n"
	}
	return s

}

// FindLeaf finds a leave from the positionMap and returns a bool
func (f *Forest) FindLeaf(leaf Hash) bool {
	_, found := f.PositionMap[leaf.Mini()]
	return found
}
