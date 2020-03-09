" Example vimrc file to bind a key to fix-imports.

nm <silent> ,a :call FixImports()<cr>

" Parse --edit output and splice in the imports.
function s:ReplaceImports(lines)
    let [start, end] = split(a:lines[0], ',')
    if end > start
        " Vim apparently has no way to delete lines.
        silent execute (start+1) . ',' . end . 'delete _'
        normal ``
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
    let errs = []
    call delete(l:err)
    if v:shell_error == 0
        call s:ReplaceImports(l:out)
    endif
    if !empty(errs)
        echohl WarningMsg
        echo join(errs)
        echohl None
    endif
endfunction
