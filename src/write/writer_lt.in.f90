submodule (h5fortran:write) writer_lt

implicit none (type, external)

contains


module procedure lt0write_r32
@writer_lt_template@
end procedure lt0write_r32

module procedure lt0write_r64
@writer_lt_template@
end procedure lt0write_r64

module procedure lt0write_i32
@writer_lt_template@
end procedure lt0write_i32

module procedure lt0write_i64
@writer_lt_template@
end procedure lt0write_i64

module procedure lt0write_char
@writer_lt_template@
end procedure lt0write_char


module procedure lt1write
@writer_lt_template@
end procedure lt1write


module procedure lt2write
@writer_lt_template@
end procedure lt2write


module procedure lt3write
@writer_lt_template@
end procedure lt3write


module procedure lt4write
@writer_lt_template@
end procedure lt4write


module procedure lt5write
@writer_lt_template@
end procedure lt5write


module procedure lt6write
@writer_lt_template@
end procedure lt6write


module procedure lt7write
@writer_lt_template@
end procedure lt7write

end submodule writer_lt
