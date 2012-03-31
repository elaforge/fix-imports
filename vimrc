" Example vimrc file to bind a key to fix-imports.

nm <silent> ,a :call FixImports()<cr>

" Run the contents of the current buffer through the fix-imports cmd.  Print
" any stderr output on the status line.
" Remove 'a' from cpoptions if you don't want this to mess up #.
function FixImports()
    let out = tempname()
    let err = tempname()
    let tmp = tempname()
    " Using a tmp file means I don't have to save the buffer, which the user
    " didn't ask for.
    execute 'write' tmp
    execute 'silent !fix-imports -v' expand('%') '<' tmp '>' out '2>' err
    let errs = readfile(err)
    if v:shell_error == 0
        " Don't replace the buffer if there's no change, this way I won't
        " mess up fold and undo state.
        call system('cmp -s ' . tmp . ' ' . out)
        if v:shell_error != 0
            " Is there an easier way to replace the buffer with a file?
            let old_line = line('.')
            let old_col = col('.')
            let old_total = line('$')
            %d
            execute 'silent :read' out
            0d
            let new_total = line('$')
            " If the import fix added or removed lines I need to take that
            " into account.  This will be wrong if the cursor was above the
            " import block.
            call cursor(old_line + (new_total - old_total), old_col)
            " The reload will forget fold state.  It was open, right?
            if foldclosed('.') != -1
                execute 'normal zO'
            endif
        endif
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
