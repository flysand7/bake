/*
    Filesystem utilitiess

    This package provides common utilities for the bake's filesystem. One of the features that this
    package offers is the file interning.
*/
package bake_fs

import fpath "core:path/filepath"
import path "core:path/slashpath"

/*
    Unique file identifier

    This number identifies a file. The number is guaranteed to uniquely identify a file, however,
    it is not guaranteed to be the same across invokations of bake. In order to obtain this id,
    the file must be registered using the `register_file` function.
*/
File_Id :: distinct u64

/*
    File information

    This struct represents information about a file that can be retrieved by id.
*/
File_Info :: struct {
    path: string,
}

/*
    Known state of the filesystem.

    Structure representing the known state of the file system. Files inside of this structure must
    be added using the `register_file` function.
*/
Fs :: struct {
    paths: [dynamic]string,
}

/*
    Return initialized Fs
*/
make_fs :: proc(a := context.allocator) -> Fs {
    return Fs {
        paths = make([dynamic]string),
    }
}

/*
    Register a file

    This registers a file in the filesystem and assigns the file a unique identification number.
    
    The working directory and filename are supplied as separate parameters, because in the context
    of bake, where the planning and the execution of the recipes are separated, and it would be wrong
    to use the current working directory during planning, before it is changed to the requested
    location.

    This function uses the allocator parameter to allocate the space needed for the normalized
    path.
*/
register_file :: proc(fs: ^Fs, workdir, filename: string, a := context.allocator) -> File_Id {
    assert(fpath.is_abs(workdir), "Working directory must be an absolute path")
    normalized_path := path.join({workdir, filename}, a)
    for file_path, file_id in fs.paths {
        if file_path == normalized_path {
            return File_Id(file_id)
        }
    }
    id := File_Id(len(fs.paths))
    append(&fs.paths, normalized_path)
    return id
}

/*
    Retrieve the file information by id.

    This retrieves the information of a registered file using its identifier.
*/
get_info_by_id :: proc(fs: ^Fs, id: File_Id) -> File_Info {
    path := fs.paths[id]
    return File_Info {
        path = path,
    }
}
