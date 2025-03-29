module CacheDir = struct
  let cachedir_tag_filename = "CACHEDIR.TAG"
  let cachedir_tag_sig = "Signature: 8a477f597d28d172789f06886806bc55"
  let cachedir_tag_msg = {|# This is a cache directory tag created by <application_name>
  # For references, see: "https://bford.info/cachedir/"|}

  let write_tag ?(template=cachedir_tag_msg) dir = 
    if Sys.is_directory dir then
      try
        let oc = dir ^ cachedir_tag_filename |> open_out in
        output_string oc (cachedir_tag_sig ^ "\n");
        output_string oc template;
        close_out oc
      with 
      | Sys_error msg -> failwith ("System error while writing: " ^ msg)
    else 
      failwith ("Invalid directory: " ^ dir)
  let is_tagged dir = 
    Sys.file_exists (Filename.concat dir cachedir_tag_filename)

  let has_tag dir = 
    if Sys.is_directory dir then
      try
        let ic = dir ^ cachedir_tag_filename |> open_in in
        let line = input_line ic in
        line = cachedir_tag_sig
      with 
      | Sys_error msg -> failwith ("System error while reading: " ^ msg)
    else 
      failwith ("Invalid directory: " ^ dir)
end