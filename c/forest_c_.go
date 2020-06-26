package main

/*
#cgo LDFLAGS: -L./ -lforest
#include <string.h>
#include <stdlib.h>
#include "forest.h"
extern leaf init_leaf(__int128 first, __int128 second);
extern minipos init_minipos(__int128 mini, uint64_t pos);
extern minipos* index_posmap(minipos* a, uint64_t idx);
extern leaf* index_leaves(leaf* a, uint64_t idx);
extern uint64_t index_positions(uint64_t* a, size_t idx);
*/
import "C" // make sure there is no empty line between this and the c code!!!
import "unsafe"

import "github.com/mit-dci/utreexo/accumulator"

func CLeafAtIdx(f *accumulator.Forest, idx uint64) C.leaf {
	oval := f.Data.Read(idx)
	var first [16]byte
	var second [16]byte
	copy(first[:], oval[:16])
	copy(second[:], oval[16:])
	val := C.init_leaf(first, second)
	return val
}

func LeafFor(ptr *C.leaf) accumulator.Hash {
	var concat [32]byte
	copy(concat[:], ptr.first[:])
	copy(concat[16:], ptr.second[:])
	return accumulator.Hash(concat)
}

func MiniPosFor(ptr *C.minipos) (accumulator.MiniHash, uint64) {
	mini := ptr.mini
	pos := ptr.pos
	var miniBytes [12]byte
	copy(miniBytes[:], mini[:12])
	goMini := accumulator.MiniHash(miniBytes)
	goPos := uint64(pos)
	return goMini, goPos
}

func ToC(f *accumulator.Forest) *C.forest {
	//fmt.Printf("toC: %v, size: %v\n", f.NumLeaves, f.Data.Size())
	//fmt.Println(f.ToString())
	if f.Data.Size() == 0 || f.NumLeaves == 0 {
		return nil
	}
	forest := (*C.forest)(C.malloc(C.sizeof_forest))
	leaf := (*C.leaf)(C.malloc(C.sizeof_leaf * C.size_t(f.Data.Size())))
	forest.leaves = leaf
	forest.leaves_size = C.size_t(f.Data.Size())
	for idx := uint64(0); idx < f.Data.Size(); idx++ {
		destPtr := unsafe.Pointer(C.index_leaves(leaf, C.uint64_t(idx)))
		val := CLeafAtIdx(f, idx)
		srcPtr := unsafe.Pointer(&val)
		C.memcpy(destPtr, srcPtr, C.sizeof_leaf)
	}

	posMap := (*C.minipos)(C.malloc(C.sizeof_minipos * C.size_t(f.NumLeaves)))
	forest.position_map = posMap

	//fmt.Println("position map", len(f.PositionMap), f.NumLeaves)
	for i := uint64(0); i < f.NumLeaves; i++ {
		key := f.Data.Read(i).Mini()
		val := f.PositionMap[key]
		var kb [16]byte
		copy(kb[:], key[:]) // key is only 12 bytes though!
		src := C.init_minipos(kb, C.uint64_t(val))
		destPtr := unsafe.Pointer(C.index_posmap(posMap, C.uint64_t(i)))
		C.memcpy(destPtr, unsafe.Pointer(&src), C.sizeof_minipos)
	}
	forest.height = C.uint8_t(f.Rows)
	forest.num_leaves = C.uint64_t(f.NumLeaves)
	return forest
}

func FromC(st *C.forest) *accumulator.Forest {
	if st == nil {
		return nil
	}
	//fmt.Printf("fromC: %v\n", st.num_leaves)
	f := accumulator.NewForest(nil)
	f.NumLeaves = uint64(st.num_leaves)
	f.Rows = uint8(st.height)
	ma := make(map[accumulator.MiniHash]uint64)
	for i := uint64(0); i < uint64(st.num_leaves); i++ {
		miniposPtr := C.index_posmap(st.position_map, C.uint64_t(i))
		mini, pos := MiniPosFor(miniposPtr)
		ma[mini] = uint64(pos)
	}
	f.PositionMap = ma

	dat := make([]accumulator.Hash, st.leaves_size)
	for i := uint64(0); i < uint64(st.leaves_size); i++ {
		leafPtr := C.index_leaves(st.leaves, C.uint64_t(i))
		dat[i] = LeafFor(leafPtr)
	}

	ram := new(accumulator.RamForestData)
	ram.M = dat
	f.Data = ram
	return f
}

//export cForestPrint
func cForestPrint(f *C.forest) *C.char {
	if f == nil {
		return C.CString("null tree")
	}
	forest := FromC(f)
	str := forest.ToString()
	str += forest.PrintPositionMap()
	return C.CString(str)
}

func cPositionsToSlice(positions *C.uint64_t, numPositions C.size_t) []uint64 {
	arr := make([]uint64, numPositions)
	for i := C.size_t(0); i < numPositions; i++ {
		arr[i] = uint64(C.index_positions(positions, i))
	}
	return arr
}

func cLeavesToSlice(hashes *C.leaf, numHashes C.size_t) []accumulator.Leaf {
	arr := make([]accumulator.Leaf, numHashes)
	for i := 0; i < int(numHashes); i++ {
		var thirtyTwo [32]byte
		indexedPtr := unsafe.Pointer(C.index_leaves(hashes, C.uint64_t(i)))
		thirtyTwoSlice := C.GoBytes(indexedPtr, 32)
		copy(thirtyTwo[:], thirtyTwoSlice[:])
		arr[i] = accumulator.Leaf{Hash: accumulator.Hash(thirtyTwo)}
	}
	return arr
}

func fromCTreeOrEmpty(f *C.forest) *accumulator.Forest {
	var forest *accumulator.Forest
	if f == nil {
		forest = accumulator.NewForest(nil)
	} else {
		forest = FromC(f)
	}
	return forest
}

//export cForestAdd
func cForestAdd(f *C.forest, hashes *C.leaf, numHashes C.size_t) *C.forest {
	forest := fromCTreeOrEmpty(f)
	arr := cLeavesToSlice(hashes, numHashes)
	_, err := forest.Modify(arr, nil)
	if err != nil {
		return nil
	}
	return ToC(forest)
}

//export cForestDelete
func cForestDelete(f *C.forest, positions *C.uint64_t, numPositions C.size_t) *C.forest {
	forest := fromCTreeOrEmpty(f)
	arr := cPositionsToSlice(positions, numPositions)
	_, err := forest.Modify(nil, arr)
	if err != nil {
		return nil
	}
	return ToC(forest)
}

//export cForestPrepareInsertion
func cForestPrepareInsertion(f *C.forest, delta C.uint64_t) *C.forest {
	forest := fromCTreeOrEmpty(f)
	err := forest.PrepareInsertion(uint64(delta))
	if err != nil {
		return nil
	}
	return ToC(forest)
}

//export cForestSwapNodes
func cForestSwapNodes(f *C.forest, from, to uint64, row uint8) *C.forest {
	forest := fromCTreeOrEmpty(f)
	if !forest.SwapNodes(accumulator.Arrow{from, to}, row) {
		return nil
	}
	return ToC(forest)
}

//export cForestHashRow
func cForestHashRow(f *C.forest, dirt *C.uint64_t, numDirt C.size_t) *C.forest {
	forest := fromCTreeOrEmpty(f)
	godirt := make([]uint64, numDirt)
	for i, _ := range godirt {
		p := (*uint64)(unsafe.Pointer(uintptr(unsafe.Pointer(dirt)) + unsafe.Sizeof(*dirt)*uintptr(i)))
		godirt[i] = *p
	}
	forest.HashRow(godirt)
	forest.ToString()
	v := ToC(forest)
	return v
}

//export cForestFree
func cForestFree(f *C.forest) {
	if f != nil {
		if f.position_map != nil {
			C.free(unsafe.Pointer(f.position_map))
		}
		if f.leaves != nil {
			C.free(unsafe.Pointer(f.leaves))
		}
		C.free(unsafe.Pointer(f))
	}
}

func main() {}
