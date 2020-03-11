" Example vimrc file to bind a key to fix-imports.

nm <silent> ,a :call FixImports()<cr>

" Parse --edit output and splice in the imports.
function s:ReplaceImports(lines)
    let [start, end] = split(a:lines[0], ',')
    " Otherwise vim does string comparison.
    if end+0 > start+0
        " This stupidity is necessary because vim apparently has no way to
        " delete lines.
        let old_line = line('.')
        let old_col = col('.')
        let old_total = line('$')
        silent execute (start+1) . ',' . end . 'delete _'
        " If the import fix added or removed lines I need to take that
        " into account.  This will be wrong if the cursor was in the
        " import block.
        let new_total = line('$')
        call cursor(old_line + (new_total - old_total), old_col)
    endif
    call append(start, a:lines[1:])
endfunction


" Run the contents of the current buffer through the fix-imports cmd.  Print
" any stderr output on the status line.
function FixImports()
    let l:err = tempname()
    let l:cmd = 'fix-imports -v --edit ' . expand('%') . ' 2>' . l:err
    let l:out = systemlist(l:cmd, bufnr('%'))

    let errs = readfile(l:err)
    call delete(l:err)
    if v:shell_error == 0
        call s:ReplaceImports(l:out)
    endif
    redraw!
    if !empty(errs)
        echohl WarningMsg
        echo join(errs)
        echohl None
    endif
endfunction
