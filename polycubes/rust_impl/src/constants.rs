use std::mem;

pub type IdxT = u16;

// Max individual cubes in a polycube
pub const MAX_SIZE:usize = 11;

// Number of bits needed to store a coordinate in 1 axis (x,y,z).
pub const COORD_BIT_LEN:usize = 4;

// Dimensions of the polycub space.
pub const DIMS:usize = 3;

// The position of a cube in a 3D grid can be represented as a vector.
// We can pack it in an integer which has enough bits to fit 3 coordinates.
pub const IDX_BIT_LEN:usize = mem::size_of::<IdxT>() * 8;

// A polycube may be represented as an array of positions
// We compute the max length of such array, with padding to align to 64bits
pub const CUBE_ARR_PAD:usize = (64/IDX_BIT_LEN) - MAX_SIZE % (64/IDX_BIT_LEN);
pub const CUBE_ARR_LEN:usize = MAX_SIZE + CUBE_ARR_PAD;

// The cardinality of the group of rotation matrices with the multiplication operator.
// No idea how I came up with this number...
pub const POSSIBLE_ROTATIONS:usize = 2 * DIMS * 4;

// If we assign a unique integer to a 3D grid where eaach axis has 2^COORD_BIT_LEN,
// Then there are IDX_MAX different values possible.
pub const IDX_MAX:usize = 2usize.pow(COORD_BIT_LEN as u32).pow(DIMS as u32);

// Max value of a position in any given axis.
pub const MAX_COORD:IdxT = (2 as IdxT).pow(COORD_BIT_LEN as u32) - 1;

// Masks used to get an axis coordinate from a position value.
pub const MASK_X:IdxT = MAX_COORD << (0*COORD_BIT_LEN);
pub const MASK_Y:IdxT = MAX_COORD << (1*COORD_BIT_LEN);
pub const MASK_Z:IdxT = MAX_COORD << (2*COORD_BIT_LEN);
pub const MASKS:[IdxT; DIMS] = [MASK_X, MASK_Y, MASK_Z];

// Shifts needed to transform a position index to a numerical axis coordinate.
pub const SHIFTS:[u32; DIMS] = [0*COORD_BIT_LEN as u32,
                                1*COORD_BIT_LEN as u32,
                                2*COORD_BIT_LEN as u32];

