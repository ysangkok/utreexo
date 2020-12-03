package accumulator

// hashableNode is the data needed to perform a hash
type hashableNode struct {
	sib, dest *polNode
	position  uint64 // doesn't really need to be there, but convenient for debugging
}

func (f *Forest) HashRow(dirtpositions []uint64) error {
	for _, hp := range dirtpositions {
		l := f.Data.Read(child(hp, f.Rows))
		r := f.Data.Read(child(hp, f.Rows) | 1)
		f.Data.write(hp, parentHash(l, r))
	}

	return nil
}
