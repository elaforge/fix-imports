" Example vimrc file to bind a key to fix-imports.

nm <silent> ,a :call FixImports()<cr>

" Parse --edit output and splice in the imports.
function s:ReplaceImports(lines)
    let [start, end] = map(split(a:lines[0], ','), 'str2nr(v:val)')
    if end > start
        call deletebufline('%', start+1, end)
    endif
    call appendbufline('%', start, a:lines[1:])
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
