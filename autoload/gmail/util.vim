" File: autoload/gmail/util.vim
" Last Modified: 2012.08.10
" Author: yuratomo (twitter @yusetomo)

function! gmail#util#message(msg)
  echo 'Gmail: ' . a:msg
  redraw
  call gmail#win#log('INF ' . a:msg)
endfunction

function! gmail#util#error(msg)
  echoerr 'Gmail: ' . a:msg
  call gmail#win#log('ERR ' . a:msg)
endfunction

function! gmail#util#confirm(msg)
  while 1
    let ans = input(a:msg)
    if ans == ''
      return 0
    elseif ans == 'y'
      return 1
    elseif ans == 'n'
      return 0
    endif
  endwhile
endfunction

function! gmail#util#response(vp, end, timeout)
  let cnt = 0
  let res = ''
  let end = 0
  while !a:vp.stdout.eof
    let line = substitute(substitute(a:vp.stdout.read(), nr2char(10), nr2char(13), 'g')
      \ , nr2char(13).nr2char(13), nr2char(13), 'g')
    if line != ''
      let res = res . line
      for line2 in split(line, "\r")
        if line2 =~ a:end
          let end = 1
          break
        endif
      endfor
      if end == 1
        break
      endif
    else
      sleep 10ms
      let cnt += 1
      if cnt >= a:timeout/10
        call gmail#util#message('request timeout!!')
        return []
      endif
    endif
  endwhile
  return split(res, "\r")
endfunction

function! gmail#util#encodeUtf7(str)
  let mod1 = iconv(a:str, &enc, 'UTF-7')
  let mod2 = substitute(mod1, '/', ',', 'g')
  let mod3 = substitute(mod2, '&', '&-', 'g')
  return     substitute(mod3, '+', '&', '')
endfunction

function! gmail#util#decodeUtf7(str)
  let mod1 = substitute(a:str, '&', '+', '')
  let mod2 = substitute(mod1, '&-', '&', 'g')
  let mod3 = substitute(mod2, ',', '/', 'g')
  return     iconv(mod3, 'UTF-7', &enc)
endfunction

function! gmail#util#encodeMime(str)
  return
    \ '=?' .
    \ g:gmail_default_encoding .
    \ '?B?' .
    \ s:encodeBase64Str(iconv(a:str, &enc, g:gmail_default_encoding)) . 
    \ '?='
endfunction

function! gmail#util#decodeMime(str)
  let end   = strridx(a:str, '?=')
  let start = strridx(a:str, '?', end-1)
  if start == -1 || end == -1
    return a:str
  endif

  let enc_e = strridx(a:str, '?', start-1)
  let enc_s = strridx(a:str, '=?', enc_e-1)
  if enc_s == -1
    let enc = g:gmail_default_encoding
  else
    let enc = a:str[ enc_s+2 : enc_e-1 ]
  endif
  return gmail#util#decodeBase64_with_iconv(a:str[ start+1 : end-1 ], enc, &enc)
endfunction

let s:standard_table = [
      \ "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P",
      \ "Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f",
      \ "g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v",
      \ "w","x","y","z","0","1","2","3","4","5","6","7","8","9","+","/"]

let g:jismap = {
  \ "45"  : [ "①", "②", "③", "④", "⑤", "⑥", "⑦", "⑧", "⑨", "⑩", "⑪", "⑫", "⑬", "⑭", "⑮", "⑯", "⑰", "⑱", "⑲", "⑳", "Ⅰ", "Ⅱ", "Ⅲ", "Ⅳ", "Ⅴ", "Ⅵ", "Ⅶ", "Ⅷ", "Ⅸ", "Ⅹ", "",   "㍉", "㌔", "㌢", "㍍", "㌘", "㌧", "㌃", "㌶", "㍑", "㍗", "㌍", "㌦", "㌣", "㌫", "㍊", "㌻", "㎜", "㎝", "㎞", "㎎", "㎏", "㏄", "㎡", "",   "",   "",   "",   "",   "",   "",   "",   "㍻", "〝", "〟", "№", "㏍", "℡", "㊤", "㊥", "㊦", "㊧", "㊨", "㈱", "㈲", "㈹", "㍾", "㍽", "㍼", "≒", "≡", "∫", "∮", "∑", "√", "⊥", "∠", "∟", "⊿", "∵", "∩", "∪", "",   "" ],
  \ "121" : [ "纊", "褜", "鍈", "銈", "蓜", "俉", "炻", "昱", "棈", "鋹", "曻", "彅", "丨", "仡", "仼", "伀", "伃", "伹", "佖", "侒", "侊", "侚", "侔", "俍", "偀", "倢", "俿", "倞", "偆", "偰", "偂", "傔", "僴", "僘", "兊", "兤", "冝", "冾", "凬", "刕", "劜", "劦", "勀", "勛", "匀", "匇", "匤", "卲", "厓", "厲", "叝", "﨎", "咜", "咊", "咩", "哿", "喆", "坙", "坥", "垬", "埈", "埇", "﨏", "塚", "增", "墲", "夋", "奓", "奛", "奝", "奣", "妤", "妺", "孖", "寀", "甯", "寘", "寬", "尞", "岦", "岺", "峵", "崧", "嵓", "﨑", "嵂", "嵭", "嶸", "嶹", "巐", "弡", "弴", "彧", "德" ],
  \ "122" : [ "忞", "恝", "悅", "悊", "惞", "惕", "愠", "惲", "愑", "愷", "愰", "憘", "戓", "抦", "揵", "摠", "撝", "擎", "敎", "昀", "昕", "昻", "昉", "昮", "昞", "昤", "晥", "晗", "晙", "晴", "晳", "暙", "暠", "暲", "暿", "曺", "朎", "朗", "杦", "枻", "桒", "柀", "栁", "桄", "棏", "﨓", "楨", "﨔", "榘", "槢", "樰", "橫", "橆", "橳", "橾", "櫢", "櫤", "毖", "氿", "汜", "沆", "汯", "泚", "洄", "涇", "浯", "涖", "涬", "淏", "淸", "淲", "淼", "渹", "湜", "渧", "渼", "溿", "澈", "澵", "濵", "瀅", "瀇", "瀨", "炅", "炫", "焏", "焄", "煜", "煆", "煇", "凞", "燁", "燾", "犱" ],
  \ "123" : [ "犾", "猤", "猪", "獷", "玽", "珉", "珖", "珣", "珒", "琇", "珵", "琦", "琪", "琩", "琮", "瑢", "璉", "璟", "甁", "畯", "皂", "皜", "皞", "皛", "皦", "益", "睆", "劯", "砡", "硎", "硤", "硺", "礰", "礼", "神", "祥", "禔", "福", "禛", "竑", "竧", "靖", "竫", "箞", "精", "絈", "絜", "綷", "綠", "緖", "繒", "罇", "羡", "羽", "茁", "荢", "荿", "菇", "菶", "葈", "蒴", "蕓", "蕙", "蕫", "﨟", "薰", "蘒", "﨡", "蠇", "裵", "訒", "訷", "詹", "誧", "誾", "諟", "諸", "諶", "譓", "譿", "賰", "賴", "贒", "赶", "﨣", "軏", "﨤", "逸", "遧", "郞", "都", "鄕", "鄧", "釚" ],
  \ "124" : [ "釗", "釞", "釭", "釮", "釤", "釥", "鈆", "鈐", "鈊", "鈺", "鉀", "鈼", "鉎", "鉙", "鉑", "鈹", "鉧", "銧", "鉷", "鉸", "鋧", "鋗", "鋙", "鋐", "﨧", "鋕", "鋠", "鋓", "錥", "錡", "鋻", "﨨", "錞", "鋿", "錝", "錂", "鍰", "鍗", "鎤", "鏆", "鏞", "鏸", "鐱", "鑅", "鑈", "閒", "隆", "﨩", "隝", "隯", "霳", "霻", "靃", "靍", "靏", "靑", "靕", "顗", "顥", "飯", "飼", "餧", "館", "馞", "驎", "髙", "髜", "魵", "魲", "鮏", "鮱", "鮻", "鰀", "鵰", "鵫", "鶴", "鸙", "黑", "",   "",   "ⅰ", "ⅱ", "ⅲ", "ⅳ", "ⅴ", "ⅵ", "ⅶ", "ⅷ", "ⅸ", "ⅹ", "￢", "￤", "＇", "＂" ],
  \}

function! s:encodeBase64Str(data)
  let b64 = s:b64encode(gmail#util#str2bytes(a:data), s:standard_table, '=')
  return join(b64, '')
endfunction

function! gmail#util#encodeBase64(bytes)
  let b64 = s:b64encode(a:bytes, s:standard_table, '=')
  return join(b64, '')
endfunction

function! gmail#util#decodeBase64(data)
  try
    let bytes = s:b64decode(split(a:data, '\zs'), s:standard_table, '=')
    return s:bytes2str(bytes)
  catch /.*/
    return a:data
  endtry
endfunction

function! s:iconv_iso2022jp_ex(data, fenc, tenc)
  let li = split(a:data, '\%(\\x1b\\x24\\x42\|\\x1b\\x28\\x42\)\zs', '1')
  let mt = 0
  let tm = []
  for item in li
    if mt == 1
      let item = substitute(item, '^\%(\\x..\\x..\)*\zs\\x\(2[dD]\|7[9a-cA-C]\)\\x\(2[1-9a-fA-F]\|[3-6][0-9a-fA-F]\|7[0-9a-eA-E]\)\ze', '\="\\x1b\\x28\\x42[{".submatch(1).submatch(2)."]}\\x1b\\x24\\x42"', 'g')
    endif
    if item =~ '\\x1b\\x24\\x42$'
      let mt = 1
    else
      let mt = 0
    endif
    call add(tm, item)
  endfor
  let hexstr = eval( '"'. join(tm, '') . '"')
  let hexstr = iconv(hexstr, a:fenc, a:tenc)
  let ret = substitute(hexstr, '\[{\(2[dD]\|7[9a-cA-C]\)\(2[1-9a-fA-F]\|[3-6][0-9a-fA-F]\|7[0-9a-eA-E]\)\]}', '\=g:jismap[eval("0x".submatch(1))][eval("0x".submatch(2)) - 0x21]', 'g')
  return ret
endfunction

function! gmail#util#iconv_7bit(data, fenc, tenc)
  try
    let hexstr = a:data
    if a:fenc =~? 'ISO-2022-JP'
      let ret = s:iconv_iso2022jp_ex(join(gmail#util#str2hexstr(hexstr), ''), a:fenc, a:tenc)
    else
      let ret = iconv(hexstr, a:fenc, a:tenc)
    endif
    return ret
  catch /.*/
    call gmail#win#log('ERROR gmail#util#iconv_7bit():' . v:exception)
    return a:data
  endtry
endfunction

function! gmail#util#decodeBase64_with_iconv(data, fenc, tenc)
  try
    let bytes = s:b64decode(split(a:data, '\zs'), s:standard_table, '=')
    let hexstr = s:bytes2hexstr(bytes)
    if a:fenc =~? 'ISO-2022-JP'
      let ret = s:iconv_iso2022jp_ex(hexstr, a:fenc, a:tenc)
    else
      let ret = iconv(eval( '"'. hexstr . '"'), a:fenc, a:tenc)
    endif
    return ret
  catch /.*/
    call gmail#win#log('ERROR gmail#util#decodeBase64_with_iconv():' . v:exception)
    return a:data
  endtry
endfunction

function! gmail#util#saveAttachedFile(data, type, dir, fname)
  try
    let bytes = s:b64decode(split(a:data, '\zs'), s:standard_table, '=')
    let hexstr = s:bytes2hexstr(bytes)
    let hlist = map(split(
      \ hexstr,
      \ '\\x0[aA]', 1),
      \ 'eval(''"'' . substitute(v:val, ''\\x00'', ''\\x0A'', ''g'') . ''"'')')
    let savefile = a:dir . a:fname
    if filewritable(savefile)
      let prompt = 'Overwrite? [y]es, [c]ancel, [t]imestap : '
      echo 'Exists ' . savefile
      let pret = substitute(input(prompt), '^[^:]\+: ', '', '')
      if pret =~? '^c'
        echo 'Cancel'
        return
      elseif pret =~? '^t'
        let savefile = a:dir . strftime('%Y%m%d_%H%M%S_') . a:fname
      elseif pret !~? '^y'
        echo 'Cancel'
        return
      endif
    endif
    call writefile(hlist, savefile, 'b')
    echo "Saved " . savefile
  catch /.*/
    echoerr v:exception
    return 0
  endtry
endfunction

function! s:b64decode(b64, table, pad)
  let a2i = {}
  for i in range(len(a:table))
    let a2i[a:table[i]] = i
  endfor
  let bytes = []
  for i in range(0, len(a:b64) - 1, 4)
    let n = a2i[a:b64[i]] * 0x40000
          \ + a2i[a:b64[i + 1]] * 0x1000
          \ + (a:b64[i + 2] == a:pad ? 0 : a2i[a:b64[i + 2]]) * 0x40
          \ + (a:b64[i + 3] == a:pad ? 0 : a2i[a:b64[i + 3]])
    call add(bytes, n / 0x10000)
    call add(bytes, n / 0x100 % 0x100)
    call add(bytes, n % 0x100)
  endfor
  if a:b64[-1] == a:pad
    unlet bytes[-1]
  endif
  if a:b64[-2] == a:pad
    unlet bytes[-1]
  endif
  return bytes
endfunction

function! s:b64encode(bytes, table, pad)
  let b64 = []
  for i in range(0, len(a:bytes) - 1, 3)
    let n = a:bytes[i] * 0x10000
          \ + get(a:bytes, i + 1, 0) * 0x100
          \ + get(a:bytes, i + 2, 0)
    call add(b64, a:table[n / 0x40000])
    call add(b64, a:table[n / 0x1000 % 0x40])
    call add(b64, a:table[n / 0x40 % 0x40])
    call add(b64, a:table[n % 0x40])
  endfor
  if len(a:bytes) % 3 == 1
    let b64[-1] = a:pad
    let b64[-2] = a:pad
  endif
  if len(a:bytes) % 3 == 2
    let b64[-1] = a:pad
  endif
  return b64
endfunction

function! s:bytes2str(bytes)
  return eval('"' . join(map(copy(a:bytes), 'printf(''\x%02x'', v:val)'), '') . '"')
endfunction

function! s:bytes2hexstr(bytes)
  return join(map(copy(a:bytes), 'printf(''\x%02x'', v:val)'), '')
endfunction

function! gmail#util#str2bytes(str)
  return map(range(len(a:str)), 'char2nr(a:str[v:val])')
endfunction

function! gmail#util#str2hexstr(str)
  return map(range(len(a:str)), 'printf(''\x%02x'', char2nr(a:str[v:val]))')
endfunction

function! gmail#util#decodeQuotedPrintable(data)
  return substitute(a:data, '=\(\x\x\|\n\)', '\=submatch(1)=="\n"?"":eval(''"''."\\x".submatch(1).''"'')', 'g')
endfunction

function! gmail#util#neglect_htmltag()
  setl modifiable
  :%s/<.\{-\}>//ge
  :%s/^\s*//ge
  :%s/^\s*$//ge
  :%s/\n\n\n//ge
  :%s/&quot;/"/ge
  :%s/&laquo;/竕ｪ/ge
  :%s/&raquo;/竕ｫ/ge
  :%s/&lt;/</ge
  :%s/&gt;/>/ge
  :%s/&amp;/\&/ge
  :%s/&yen;/\\/ge
  :%s/&cent;/ﾂ｢/ge
  :%s/&copy;/c/ge
  :%s/&apos;/'/ge
  :%s/&nbsp;/ /ge
  :%s/&mdash;/--/ge
  redraw
  setl nomodifiable
endfunction

