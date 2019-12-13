package utreexo

import (
	"fmt"
	"math/rand"
	"testing"
)

func TestPollardRand(t *testing.T) {
	// for z := 0; z < 10000; z++ {
	z := 44
	rand.Seed(int64(z))
	fmt.Printf("randseed %d\n", z)
	err := pollardRandomRemember(8)
	if err != nil {
		// fmt.Printf("randseed %d\n", z)
		t.Fatal(err)
	}
	// }
}

func TestPollardFixed(t *testing.T) {
	rand.Seed(2)
	//	err := pollardMiscTest()
	//	if err != nil {
	//		t.Fatal(err)
	//	}
	//	for i := 6; i < 100; i++ {
	err := fixedPollard(11)
	if err != nil {
		t.Fatal(err)
	}
}

func TestPollardGrab(t *testing.T) {

	var p Pollard
	// they're all forgettable
	adds := make([]LeafTXO, 15)
	for j, _ := range adds {
		adds[j].Hash[0] = uint8(j)
		adds[j].Hash[2] = uint8(0xff)
		adds[j].Remember = true
	}

	// apply adds and deletes to the bridge node (could do this whenever)
	err := p.Modify(adds, nil)
	if err != nil {
		t.Fatal(err)
	}
	fmt.Printf(p.toString())

	for i := uint64(0); i < 29; i++ {
		par, parsib, lr, err := p.grabPos2(i)
		if err != nil {
			fmt.Println(err.Error())
			continue
		}
		if par == nil {
			fmt.Printf("pos %d n lr %d which is %x\n",
				i, lr, p.tops[lr].data[:4])

		} else {
			fmt.Printf("pos %d n %x nsib %x lr %d\n",
				i, par.data[:4], parsib.data[:4], lr)
		}

	}

}

func pollardRandomRemember(blocks int32) error {
	f := NewForest()

	var p Pollard

	// p.Minleaves = 0

	sn := NewSimChain(0x07)
	sn.lookahead = 400
	for b := int32(0); b < blocks; b++ {
		adds, delHashes := sn.NextBlock(rand.Uint32() & 0x03)

		fmt.Printf("\t\t\tstart block %d del %d add %d - %s\n",
			sn.blockHeight, len(delHashes), len(adds), p.Stats())

		// get proof for these deletions (with respect to prev block)
		bp, err := f.ProveBlock(delHashes)
		if err != nil {
			return err
		}

		// verify proofs on rad node
		err = p.IngestBlockProof(bp)
		if err != nil {
			return err
		}

		// apply adds and deletes to the bridge node (could do this whenever)
		_, err = f.Modify(adds, bp.Targets)
		if err != nil {
			return err
		}
		fmt.Printf("del %v\n", bp.Targets)

		// apply adds / dels to pollard
		err = p.Modify(adds, bp.Targets)
		if err != nil {
			return err
		}

		fmt.Printf("pol postadd %s", p.toString())

		fmt.Printf("frs postadd %s", f.toString())

		// check all leaves match
		if !p.equalToForestIfThere(f) {
			return fmt.Errorf("pollard and forest leaves differ")
		}

		fullTops := f.GetTops()
		polTops := p.topHashesReverse()

		// check that tops match
		if len(fullTops) != len(polTops) {
			return fmt.Errorf("block %d full %d tops, pol %d tops",
				sn.blockHeight, len(fullTops), len(polTops))
		}
		fmt.Printf("top matching: ")
		for i, ft := range fullTops {
			fmt.Printf("f %04x p %04x ", ft[:4], polTops[i][:4])
			if ft != polTops[i] {
				return fmt.Errorf("block %d top %d mismatch, full %x pol %x",
					sn.blockHeight, i, ft[:4], polTops[i][:4])
			}
		}
		fmt.Printf("\n")
	}

	return nil
}

// fixedPollard adds and removes things in a non-random way
func fixedPollard(leaves int32) error {
	fmt.Printf("\t\tpollard test add %d remove 1\n", leaves)
	f := NewForest()

	leafCounter := uint64(0)

	dels := []uint64{2, 5, 6}

	// they're all forgettable
	adds := make([]LeafTXO, leaves)

	// make a bunch of unique adds & make an expiry time and add em to
	// the TTL map
	for j, _ := range adds {
		adds[j].Hash[1] = uint8(leafCounter)
		adds[j].Hash[2] = uint8(leafCounter >> 8)
		adds[j].Hash[3] = uint8(leafCounter >> 16)
		adds[j].Hash[4] = uint8(leafCounter >> 24)
		adds[j].Hash[9] = uint8(0xff)

		// the first utxo addded lives forever.
		// (prevents leaves from goign to 0 which is buggy)
		adds[j].Remember = true
		leafCounter++
	}

	// apply adds and deletes to the bridge node (could do this whenever)
	_, err := f.Modify(adds, nil)
	if err != nil {
		return err
	}
	fmt.Printf("forest  post del %s", f.toString())

	var p Pollard

	err = p.add(adds)
	if err != nil {
		return err
	}

	fmt.Printf("pollard post add %s", p.toString())

	err = p.rem2(dels)
	if err != nil {
		return err
	}

	_, err = f.Modify(nil, dels)
	if err != nil {
		return err
	}
	fmt.Printf("forest  post del %s", f.toString())

	fmt.Printf("pollard post del %s", p.toString())

	if !p.equalToForest(f) {
		return fmt.Errorf("p != f (leaves)\n")
	}

	return nil
}
