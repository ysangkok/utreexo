package accumulator

import (
	"fmt"
)

/* we need to be able to undo blocks!  for bridge nodes at least.
compact nodes can just keep old roots.
although actually it can make sense for non-bridge nodes to undo as well...
*/

// TODO in general, deal with numLeaves going to 0

// blockUndo is all the data needed to undo a block: number of adds,
// and all the hashes that got deleted and where they were from
type undoBlock struct {
	numAdds   uint32   // number of adds in the block
	positions []uint64 // position of all deletions this block
	hashes    []Hash   // hashes that were deleted
}

func (u *undoBlock) ToString() string {
	s := fmt.Sprintf("- uuuu undo block %d adds\t", u.numAdds)
	s += fmt.Sprintf("%d dels:\t", len(u.positions))
	if len(u.positions) != len(u.hashes) {
		s += "error"
		return s
	}
	for i, _ := range u.positions {
		s += fmt.Sprintf("%d %x,\t", u.positions[i], u.hashes[i][:4])
	}
	s += "\n"
	return s
}

// Undo : undoes one block with the undoBlock
func (f *Forest) Undo(ub undoBlock) error {

	prevAdds := uint64(ub.numAdds)
	prevDels := uint64(len(ub.hashes))
	// how many leaves were there at the last block?
	prevNumLeaves := f.NumLeaves + prevDels - prevAdds
	// run the transform to figure out where things came from
	leafMoves := floorTransform(ub.positions, prevNumLeaves, f.Rows)
	reverseArrowSlice(leafMoves)
	// first undo the leaves added in the last block
	f.NumLeaves -= prevAdds
	// clear out the hashes themselves (maybe don't need to but seems safer)
	// leaves dangling parents, but other things do that still...
	// for pos := f.numLeaves; pos < f.numLeaves+prevAdds; pos++ {
	// f.forest[pos] = empty
	// }

	fmt.Printf("\t\t### UNDO DATA\n")
	fmt.Printf("fnl %d leaf moves %d %v\n", f.NumLeaves, len(leafMoves), leafMoves)
	fmt.Printf("ub hashes %d\n", len(ub.hashes))

	// remove everything between prevNumLeaves and numLeaves from positionMap
	for p := f.NumLeaves; p < f.NumLeaves+prevAdds; p++ {
		fmt.Printf("remove %x@%d from map\n",
			f.Data.Read(p).Prefix(), f.PositionMap[f.Data.Read(p).Mini()])
		delete(f.PositionMap, f.Data.Read(p).Mini())
	}

	// also add everything past numleaves and prevnumleaves to dirt
	// which might already be there, inefficient!
	// TODO fix this dirt thing
	dirt := make([]uint64, len(leafMoves)*2)

	// place hashes starting at old post-remove numLeaves.  they're off the
	// forest bounds to the right; they will be shuffled in to the left.
	for i, h := range ub.hashes {
		if h == empty {
			return fmt.Errorf("hash %d in undoblock is empty", i)
		}
		f.Data.write(f.NumLeaves+uint64(i), h)
		dirt = append(dirt, f.NumLeaves+uint64(i))
	}

	// go through swaps in reverse order
	for i, a := range leafMoves {
		fmt.Printf("swapped %d %x, %d %x\n", a.To,
			f.Data.Read(a.To).Prefix(), a.From, f.Data.Read(a.From).Prefix())
		f.Data.swapHash(a.From, a.To)
		dirt[2*i] = a.To       // this is wrong, it way over hashes
		dirt[(2*i)+1] = a.From // also should be parents
	}

	// update positionMap.  The stuff we do want has been moved in to the forest,
	// the stuff we don't want has been moved to the right past the edge
	for p := f.NumLeaves; p < prevNumLeaves; p++ {
		fmt.Printf("put back edge %x@%d from map\n", f.Data.Read(p).Prefix(), p)
		f.PositionMap[f.Data.Read(p).Mini()] = p
	}
	for _, p := range ub.positions {
		fmt.Printf("put back internal %x@%d in map\n", f.Data.Read(p).Prefix(), p)
		f.PositionMap[f.Data.Read(p).Mini()] = p
	}
	for _, d := range dirt {
		// everything that moved needs to have its position updated in the map
		// TODO does it..?
		m := f.Data.Read(d).Mini()
		oldpos := f.PositionMap[m]
		if oldpos != d {
			fmt.Printf("update map %x %d to %d\n", m[:4], oldpos, d)
			delete(f.PositionMap, m)
			f.PositionMap[m] = d
		}
	}

	// rehash above all tos/froms
	f.NumLeaves = prevNumLeaves // change numLeaves before rehashing
	sortUint64s(dirt)
	fmt.Printf("rehash dirt: %v\n", dirt)
	err := f.reHash(dirt)
	if err != nil {
		return err
	}

	fmt.Printf("post undo forest %s\n", f.ToString())
	return nil
}

// BuildUndoData makes an undoBlock from the same data that you'd give to Modify
func (f *Forest) BuildUndoData(numadds uint64, dels []uint64) *undoBlock {
	ub := new(undoBlock)
	ub.numAdds = uint32(numadds)

	// fmt.Printf("%d del, nl %d\n", len(dels), f.numLeaves)
	ub.positions = dels // the deletion positions, in sorted order
	ub.hashes = make([]Hash, len(dels))

	// populate all the hashes from the left edge of the forest
	for i, _ := range ub.positions {
		ub.hashes[i] = f.Data.Read(f.NumLeaves + uint64(i))
		if ub.hashes[i] == empty {
			fmt.Printf("warning, wrote empty hash for position %d\n",
				ub.positions[i])
		}
	}

	return ub
}
