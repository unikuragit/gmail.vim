" File: autoload/gmail/win.vim
" Last Modified: 2012.08.10
" Author: yuratomo (twitter @yusetomo)

let s:gmail_title_prefix = 'gmail-'
let g:gmail_search_key = 'ALL'
let s:gmail_winname = [ 'mailbox', 'list', 'body', 'new', 'log' ]
let s:gmail_list_menu = '   [more] [update] [unread] [read] [archive] [delete]'
let s:gmail_body_menu = '[next] [prev] [reply] [reply_all] [forward] [unread] [save_file_all] [easy_html_view]'
let [ g:GMAIL_MODE_MAILBOX, g:GMAIL_MODE_LIST, g:GMAIL_MODE_BODY, g:GMAIL_MODE_CREATE, g:GMAIL_MODE_LOG ] = range(5)
let s:gmail_mailbox_item_count = 0

function! gmail#win#open(mode)
  let res = gmail#imap#noop()
  if empty(res) || res[-1] !~ '? OK'
    return
  endif
  call s:open(a:mode)
endfunction

function! s:open(mode)
  let pref = s:gmail_winname[a:mode]
  let bufname = s:gmail_title_prefix . pref

  let winnum = winnr('$')
  for winno in range(1, winnum)
    let bn = bufname(winbufnr(winno))
    if bn == bufname
       exe winno . "wincmd w"
       return
    endif
  endfor

  if a:mode == g:GMAIL_MODE_MAILBOX
    vert new
    vert res 25
  elseif a:mode == g:GMAIL_MODE_LIST
    new
    wincmd K
    exe 'res ' . string(g:gmail_page_size+1)
  elseif a:mode == g:GMAIL_MODE_LOG
    call s:open(g:GMAIL_MODE_MAILBOX)
    call s:open(g:GMAIL_MODE_LIST)
    botright new
    res 5
  else
    let found = 0
    let winnum = winnr('$')
    for winno in range(1, winnum)
      let bn = bufname(winbufnr(winno))
      let title_mbox = s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_MAILBOX]
      let title_list = s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_LIST]
      let title_log  = s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_LOG]
      if bn != title_mbox && bn != title_list && bn != title_log
         exe winno . "wincmd w"
         let found = 1
      endif
    endfor
    if found == 0
      botright new
      wincmd J
    endif
  endif

  silent edit `=bufname`
  call s:define_default_key_mappings(a:mode)

  setl bt=nofile noswf nowrap hidden nolist nomodifiable ft=gmail

endfunction

function! s:define_default_key_mappings(mode)
  if a:mode == g:GMAIL_MODE_BODY
    augroup gmail
      au BufDelete <buffer> call gmail#imap#exit()
      au VimLeavePre *      call gmail#imap#exit()
      nmap <silent> <buffer> <CR>      <Plug>(gmail_open)
      nmap <silent> <buffer> <BS>      <Plug>(gmail_back)
      nmap <silent> <buffer> <TAB>     <Plug>(gmail_next_menu)
      nmap <silent> <buffer> <s-TAB>   <Plug>(gmail_prev_menu)
    augroup END
  elseif a:mode == g:GMAIL_MODE_LOG
  elseif a:mode == g:GMAIL_MODE_CREATE
    augroup gmail
      nmap <silent> <buffer> <CR>      <Plug>(gmail_open)
      nmap <silent> <buffer> <TAB>     <Plug>(gmail_next_menu)
      nmap <silent> <buffer> <s-TAB>   <Plug>(gmail_prev_menu)
    augroup END
  else
    augroup gmail
      au BufDelete <buffer> call gmail#imap#exit()
      au VimLeavePre *      call gmail#imap#exit()
      nmap <silent> <buffer> <CR>      <Plug>(gmail_open)
      nmap <silent> <buffer> <TAB>     <Plug>(gmail_next_menu)
      nmap <silent> <buffer> <s-TAB>   <Plug>(gmail_prev_menu)
      nmap <silent> <buffer> u         <Plug>(gmail_update)
      nmap <silent> <buffer> <s-u>     <Plug>(gmail_update_all)
      nmap <silent> <buffer> c         <Plug>(gmail_new_mail)
      nmap <silent> <buffer> a         <Plug>(gmail_select_all)
      nmap <silent> <buffer> <space>   <Plug>(gmail_select_and_next)
      nmap <silent> <buffer> <s-space> <Plug>(gmail_select_and_prev)
      nmap <silent> <buffer> dd        <Plug>(gmail_delete)
      nmap <silent> <buffer> r         <Plug>(gmail_mark_readed)
      nmap <silent> <buffer> R         <Plug>(gmail_mark_unread)
      nmap <silent> <buffer> x         <Plug>(gmail_archive)
    augroup END
  endif
endfunction

function! gmail#win#setline(idx, txt)
  setl modifiable
  call setline(a:idx, a:txt)
  setl nomodifiable
endfunction

function! gmail#win#clear()
  setl modifiable
  % delete _
  setl nomodifiable
endfunction

function! gmail#win#log(msg)
  if get(g:, 'gmail_show_log_window', 0) != 1
    return
  endif

  let back = gmail#win#mode()
  call s:open(g:GMAIL_MODE_LOG)
  setl modifiable
  call setline(line('$')+1, a:msg)
  call cursor(line('$'), 0)
  redraw
  setl nomodifiable
  call s:open(back)
endfunction

function! gmail#win#hilightLine(name, line)
  call clearmatches()
  redraw
  call matchadd(a:name, '\%' . a:line . 'l')
  redraw
endfunction

function! gmail#win#mode()
  let bufname = bufname('%')
  if bufname =~ s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_MAILBOX]
    return g:GMAIL_MODE_MAILBOX
  elseif bufname =~ s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_LIST]
    return g:GMAIL_MODE_LIST
  elseif bufname =~ s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_BODY]
    return g:GMAIL_MODE_BODY
  elseif bufname =~ s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_CREATE]
    return g:GMAIL_MODE_CREATE
  elseif bufname =~ s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_LOG]
    return g:GMAIL_MODE_LOG
  endif
  return -1
endfunction

function! gmail#win#all_close()
  exe 'silent! bd ' . s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_MAILBOX]
  exe 'silent! bd ' . s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_LIST]
  exe 'silent! bd ' . s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_BODY]
  exe 'silent! bd ' . s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_CREATE]
  exe 'silent! bd ' . s:gmail_title_prefix . s:gmail_winname[g:GMAIL_MODE_LOG]
endfunction

function! gmail#win#select(line, direct, mark)
  if gmail#win#mode() != g:GMAIL_MODE_LIST
    return
  endif
  if type(a:line) == type("")
    let line = line(a:line)
    if line == 1
      return
    endif
  else
    let line = a:line
  endif

  let l = getline(line)
  if len(l) > 0
    if a:mark == ''
      if l[0] == '>'
        let l = ' ' . l[1:]
      else
        let l = '>' . l[1:]
      endif
    else
      let l = a:mark . l[1:]
    endif
    call gmail#win#setline(line, l)

    if get(g:, 'gmail_nomove_after_select', 0) == 1
      return
    endif

    call cursor(line+a:direct, 0)
  endif
endfunction

function! gmail#win#select_all()
  if gmail#win#mode() != g:GMAIL_MODE_LIST
    return
  endif

  let line = getline('.')
  if line[0] == '>'
    let mark = ' '
  else
    let mark = '>'
  endif

  for l in range(2, line('$'))
    call gmail#win#select(l, 0, mark)
  endfor
endfunction

function! gmail#win#get_selections()
  let ids = []
  if gmail#win#mode() != g:GMAIL_MODE_LIST
    return ids
  endif

  for line in getline(1, line('$'))
    if line[0] == '>'
      let item = split(line[2:], ' ')
      call add(ids, item[0])
    endif
  endfor
  return ids
endfunction

function! gmail#win#tab(direct)
  let mode = gmail#win#mode()
  let l = line('.')
  if l > 1
    if mode == g:GMAIL_MODE_LIST
      if a:direct == 1
        call cursor(1, 3)
      else
        call cursor(1, 1) 
        call cursor(1, col('$'))
      endif
    elseif mode == g:GMAIL_MODE_BODY
      if a:direct == 1
        call cursor(1, 1)
      else
        call cursor(1, 1) 
        call cursor(1, col('$'))
      endif
    endif
    return
  endif

  if mode == g:GMAIL_MODE_LIST
    call cursor(1, 0)
  elseif mode == g:GMAIL_MODE_BODY
    call cursor(1, 0)
  else
    return
  endif

  if a:direct == 1
    call feedkeys('f[', 'n')
  else
    call feedkeys('F[', 'n')
  endif
endfunction

function! gmail#win#back()
  if gmail#win#mode() == g:GMAIL_MODE_CREATE
    call gmail#win#open(g:GMAIL_MODE_BODY)
  endif
endfunction

function! gmail#win#update_mailboxs(mode)
  call gmail#win#open(g:GMAIL_MODE_MAILBOX)
  call gmail#win#clear()
  call gmail#win#setline(1, gmail#imap#list(a:mode))
endfunction

function! gmail#win#update_cur_mailbox(mb)
  try
    let mailbox = gmail#imap#get_mailbox()
    let unseen = gmail#imap#status_unseen(mailbox[a:mb-1].name)
    if unseen > 0
      let unseen = '(' . unseen . ')'
    else
      let unseen = '(0)'
    endif
    let line = mailbox[a:mb-1].dname . unseen
    call gmail#imap#set_mailbox_line(a:mb-1, line)
    call gmail#win#setline(a:mb, line)
  catch /.*/
  endtry
endfunction

function! gmail#win#select_mailbox(mb)
  let s:gmail_mailbox_item_count = gmail#imap#select(a:mb)
  call gmail#win#hilightLine('gmailSelect', a:mb+1)
  call gmail#win#setline(a:mb+1, gmail#imap#mailbox_line(a:mb))
  redraw
endfunction

function! s:reselect()
  call gmail#win#open(g:GMAIL_MODE_MAILBOX)
  let s:gmail_mailbox_item_count = gmail#imap#select(gmail#imap#mailbox_index())
endfunction

function! gmail#win#newly_list()
  call s:reselect()
  call gmail#win#open(g:GMAIL_MODE_LIST)
  let newly_uids = gmail#imap#search(g:gmail_search_key)

  call gmail#win#open(g:GMAIL_MODE_LIST)
  if len(newly_uids) > len(s:gmail_uids)
    let new_message_num = len(newly_uids) - len(s:gmail_uids)
    let fs = newly_uids[-new_message_num]
    let fe = newly_uids[-1]
    let s:gmail_uids = newly_uids
    let old_list = s:gmail_list
    let s:gmail_list = old_list[0 : 0]
    call extend(s:gmail_list, gmail#imap#fetch_header(fs, fe))
    call extend(s:gmail_list, old_list[1 : ])
    call gmail#win#clear()
    call gmail#win#setline(1, s:gmail_list)
    redraw
  else
    call gmail#util#message('No new messages.')
  endif
endfunction

function! s:clear_list()
  if exists('s:gmail_list')
    unlet s:gmail_list
  endif
  if exists('s:gmail_uids')
    unlet s:gmail_uids
  endif
  let s:gmail_page = -1
endfunction

function! gmail#win#update_list(page, clear)
  if a:clear
    call s:clear_list()
  endif

  call gmail#win#open(g:GMAIL_MODE_LIST)
  call clearmatches()

  if !exists('s:gmail_page')
    let s:gmail_page = -1
  endif

  if !exists('s:gmail_list') || s:gmail_page != a:page
    if !exists('s:gmail_uids')
      if a:clear
        let s:gmail_uids = gmail#imap#search(g:gmail_search_key)
      else
        let s:gmail_uids = range(1, s:gmail_mailbox_item_count)
      endif
    endif
    if empty(s:gmail_uids)
      call gmail#win#clear()
      return
    endif

    let last = len(s:gmail_uids)
    let is = last - g:gmail_page_size*a:page - g:gmail_page_size
    let ie = last - g:gmail_page_size*a:page - 1
    if is < 0
      let is = 0
    endif
    if ie < 0
      let ie = 0
    endif
    let fs = s:gmail_uids[is]
    let fe = s:gmail_uids[ie]
    if a:page == 0
      let s:gmail_list = []
      call insert(s:gmail_list, s:gmail_list_menu . ' search:' . g:gmail_search_key, 0)
    endif
    call extend(s:gmail_list, gmail#imap#fetch_header(fs, fe))
  endif

  call gmail#win#clear()
  call gmail#win#setline(1, s:gmail_list)
  redraw

  if a:page > 0
    call cursor(line('$'), 0)
  endif
  let s:gmail_page = a:page

endfunction

function! gmail#win#click()
  let head = gmail#imap#get_header()
  let l = line('.')
  if gmail#win#mode() == g:GMAIL_MODE_MAILBOX
    call gmail#win#select_mailbox(l-1)
    call gmail#win#update_list(0, 1)
  elseif gmail#win#mode() == g:GMAIL_MODE_LIST
    if l == 1
      let menu = expand('<cword>')
      if menu == 'more'
        call gmail#win#more_list()
      elseif menu == 'update'
        call gmail#win#newly_list()
      elseif menu == 'unread'
        call gmail#win#mark_unread()
      elseif menu == 'read'
        call gmail#win#mark_readed()
      elseif menu == 'delete'
        call gmail#win#delete()
      elseif menu == 'archive'
        call gmail#win#archive()
      endif
    else
      call s:select_and_show_body(l)
    endif
  elseif gmail#win#mode() == g:GMAIL_MODE_BODY
    if l == 1
      let menu = expand('<cword>')
      if menu == 'next'
        call gmail#win#next()
      elseif menu == 'prev'
        call gmail#win#prev()
      elseif menu == 'reply'
        call gmail#smtp#open(head.Return_Path, [], 'Re:' . head.Subject, s:replyBody())
      elseif menu == 'reply_all'
        call gmail#smtp#open(head.Return_Path, head.Cc, 'Re:' . head.Subject, s:replyBody())
      elseif menu == 'forward'
        call gmail#smtp#open('', [], 'Fw:' . head.Subject, s:replyBody())
      elseif menu == 'unread'
        let id = s:get_previewed_id()
        if id != -1
          call gmail#imap#store_seen(id, 0)
          call gmail#win#clear()
          call s:reselect()
          call gmail#win#update_list(0, 1)
          call cursor(s:gmail_previewed_list_line, 0)
        endif
      elseif menu == 'easy_html_view'
        call gmail#util#neglect_htmltag()
      elseif menu == 'save_file_all'
        if len(head.AttachmentFile) == 0
          echo "Nothing attachment file."
        else
          let savedir = g:gmail_attachment_file_save_directory
          for atfdic in head.AttachmentFile
            call gmail#util#saveAttachedFile(atfdic.fdata, atfdic.type, savedir, atfdic.fn)
          endfor
        endif
      endif
    elseif (l - 1) <= len(head.AttachmentFile)
      let prompt = 'Open : [e]dit, [s]ystem : '
      let pret = ( has('windows') ? substitute(input(prompt), '^[^:]\+: ', '', '') : 'e' )
      if pret !~? '^\%(e\|s\)'
        echo 'Cancel'
      endif
      let atfdic = head.AttachmentFile[l - 2]
      let dir = fnamemodify(expand($TMP), ':p:h') . (has('win32') ? '\' : '/')
      let file = fnamemodify(tempname(), ':t:r') . '_' . atfdic.fn
      let savefile = gmail#util#saveAttachedFile(atfdic.fdata, atfdic.type, dir, file)
      if savefile != ''
        if pret =~? '^e'
          execute 'tabe ' . savefile
        elseif pret =~? '^s'
          execute (has('win32') ? ':!start ' : ( has('win32unix') ? ':!cygstart ' : ':! ' )) . savefile
        endif
      endif
    endif
  elseif gmail#win#mode() == g:GMAIL_MODE_CREATE
    if l == 1
      if expand('<cword>') == 'send'
        if gmail#util#confirm('Send e-mail. Are you OK?[y/n]:') == 0
          call gmail#util#message('Cancel send...')
          return
        endif
        call gmail#smtp#send()
      endif
    endif
  endif
endfunction

function! s:replyBody()
  return map(s:last_list, '">" . v:val')
endfunction

function! gmail#win#update()
  if gmail#win#mode() == g:GMAIL_MODE_MAILBOX
    call gmail#win#update_cur_mailbox(line('.'))
  elseif gmail#win#mode() == g:GMAIL_MODE_LIST
    call gmail#win#update_list(0, 1)
  endif
endfunction

function! gmail#win#update_all()
  if gmail#win#mode() == g:GMAIL_MODE_MAILBOX
    for l in range(1,line('$'))
      call gmail#win#update_cur_mailbox(l)
    endfor
  endif
endfunction

function! gmail#win#search()
  if gmail#win#mode() == g:GMAIL_MODE_LIST
    let g:gmail_search_key = input('search key:', g:gmail_search_key)
    call gmail#win#update_list(0, 1)
  endif
endfunction

function! gmail#win#more_list()
  call gmail#win#update_list(s:gmail_page+1, 0)
endfunction

function! gmail#win#next()
  call gmail#win#open(g:GMAIL_MODE_LIST)
  let last = line('$')
  let l = s:gmail_previewed_list_line
  if l == last
    call gmail#win#more_list()
    let last = line('$')
    if l == last
      call gmail#win#open(g:GMAIL_MODE_BODY)
      return
    endif
  endif
  let l += 1

  call cursor(l, 0)
  call s:select_and_show_body(l)
  call gmail#win#open(g:GMAIL_MODE_BODY)
endfunction

function! gmail#win#prev()
  let l = s:gmail_previewed_list_line
  if l == 1
    return
  endif
  let l -= 1

  call gmail#win#open(g:GMAIL_MODE_LIST)
  call cursor(l, 0)
  call s:select_and_show_body(l)
  call gmail#win#open(g:GMAIL_MODE_BODY)
endfunction

function! s:get_uid(l)
  let line = getline(a:l)
  let items = split(line[2:], ' ')
  return items[0]
endfunction

function! s:select_and_show_body(l)
  call gmail#win#hilightLine('gmailSelect', a:l)
  let cline = getline(a:l)
  let uid = s:get_uid(a:l)
  call gmail#win#setline(a:l, '  ' . cline[2:])
  let s:gmail_previewed_list_line = a:l
  let s:gmail_previewed_id = uid
  call s:show_body(uid)
endfunction

function! s:get_previewed_id()
  if !exists('s:gmail_previewed_id')
    return -1
  endif
  return s:gmail_previewed_id
endfunction

function! s:show_body(id)
  call gmail#win#open(g:GMAIL_MODE_BODY)
  call gmail#win#clear()
  let s:last_list = gmail#imap#fetch_body(a:id)
  call gmail#win#setline(1, s:gmail_body_menu)
  call gmail#win#setline(2, s:last_list)
  call gmail#util#message('show message normally.')
endfunction

function! gmail#win#delete()
  if gmail#win#mode() != g:GMAIL_MODE_LIST
    return
  endif
  let [cl,cc] = [ line('.'), col('.') ]

  let message = 'Delete selected items. Are you OK?[y/n]:'
  let ids = gmail#win#get_selections()
  if empty(ids)
    let l = line('.')
    if l == 1
      call gmail#util#message('Please select an item by space key.')
      return
    endif
    unlet ids
    let ids = s:get_uid(l)
    let message = 'Delete item under the cursor. Are you OK?[y/n]:'
  endif
  echoerr string(ids)

  if gmail#util#confirm(message) == 0
    call gmail#util#message('Cancel delete...')
    return
  endif
  if gmail#imap#delete(ids) == 0
    return
  endif
  call s:reselect()
  call gmail#win#update_list(0, 1)
  call cursor(cl, cc)

  call gmail#util#message('deleted!!')
endfunction

function! gmail#win#mark_readed()
  if gmail#win#mode() != g:GMAIL_MODE_LIST
    return
  endif
  let [cl,cc] = [ line('.'), col('.') ]

  let ids = gmail#win#get_selections()
  if empty(ids)
    let l = line('.')
    if l == 1
      call gmail#util#message('Please select an item by space key.')
      return
    endif
    unlet ids
    let ids = s:get_uid(l)
  endif

  call gmail#imap#store_seen(ids, 1)
  call s:reselect()
  call gmail#win#update_list(0, 1)
  call cursor(cl, cc)

  call gmail#util#message('mark read!!')
endfunction

function! gmail#win#mark_unread()
  if gmail#win#mode() != g:GMAIL_MODE_LIST
    return
  endif
  let [cl,cc] = [ line('.'), col('.') ]

  let ids = gmail#win#get_selections()
  if empty(ids)
    let l = line('.')
    if l == 1
      call gmail#util#message('Please select an item by space key.')
      return
    endif
    unlet ids
    let ids = s:get_uid(l)
  endif

  call gmail#imap#store_seen(ids, 0)
  call s:reselect()
  call gmail#win#update_list(0, 1)
  call cursor(cl, cc)

  call gmail#util#message('mark unread!!')
endfunction

function! gmail#win#archive()
  if gmail#win#mode() != g:GMAIL_MODE_LIST
    return
  endif
  let [cl,cc] = [ line('.'), col('.') ]

  let message = 'Archive selected files. Are you OK?[y/n]:'
  let ids = gmail#win#get_selections()
  if empty(ids)
    let l = line('.')
    if l == 1
      call gmail#util#message('Please select an item by space key.')
      return
    endif
    unlet ids
    let ids = s:get_uid(l)
    let message = 'Archive item under the cursor. Are you OK?[y/n]:'
  endif

  if gmail#util#confirm(message) == 0
    call gmail#util#message('Cancel archive...')
    return
  endif
  call gmail#imap#archive(ids)
  call s:reselect()
  call gmail#win#update_list(0, 1)
  call cursor(cl, cc)

  call gmail#util#message('archived!!')
endfunction
