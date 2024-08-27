# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  ./nix_api_types,
  ./nix_api_util

proc newException*(ctx: NixContext): ref NixException =
  new result
  var
    n: cuint
    p = err_msg(NixContext(nil), ctx, addr n)
  result.msg.setLen(n)
  if n > 0:
    copyMem(result.msg[0].addr, p, result.msg.len)

template checkError*(code: nix_err) =
  if code != NIX_OK: raise newException(nix)

template mitNix*(body: untyped): untyped =
  ## Mit nix machen.
  block:
    var nix {.inject.} = c_context_create()
    defer: c_context_free(nix)
    body
    checkError err_code(nix)
