" Example vimrc file to bind a key to fix-imports.

nm <silent> ,a :call FixImports()<cr>

" Parse --edit output and splice in the imports.
function s:ReplaceImports(lines)
    let [start, end] = split(a:lines[0], ',')
    " +0 or vim does string comparison.
    if end+0 > start+0
        " This stupidity is necessary because vim apparently has no way to
        " delete lines.
        let old_line = line('.')
        let old_col = col('.')
        let old_total = line('$')
        silent execute (start+1) . ',' . end . 'delete _'
        let new_total = line('$')
        " Try to retain the cursor position.
        " If <0, then I'm inside the import block and I can just keep the line.
        " Otherwise, I have to move down for added lines or up for removed.
        let dest_line = old_line + (new_total - old_total)
        if dest_line >= 0
            call cursor(dest_line, old_col)
            call append(start, a:lines[1:])
        else
            call append(start, a:lines[1:])
            call cursor(old_line, old_col)
        endif
    else
        " This means no existing import block, just add it.
        call append(start, a:lines[1:])
    endif
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
