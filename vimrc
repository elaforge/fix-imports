" Example vimrc file to bind a key to FixImports.

nm <silent> ,a :call FixImports()<cr>

" Run the contents of the current buffer through the FixImports cmd.  Print
" any stderr output on the status line.
function FixImports()
    let out = tempname()
    let err = tempname()
    let tmp = tempname()
    " Using a tmp file means I don't have to save the buffer, which the user
    " didn't ask for.
    execute 'write' tmp
    execute 'silent !FixImports' expand('%') '<' tmp '>' out '2>' err
    let errs = readfile(err)
    if v:shell_error == 0
        " Is there an easier way to replace the buffer with a file?
        let old_line = line('.')
        let old_col = col('.')
        %d
        execute 'silent :read' out
        0d
        call cursor(old_line, old_col)
    endif
    call delete(out)
    call delete(err)
    call delete(tmp)
    redraw!
    if !empty(errs)
        echohl WarningMsg
        echo join(errs)
        echohl None
    endif
endfunction

