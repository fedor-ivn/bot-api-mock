module Server.Context (Context (Context, token)) where

import Server.Token (Token)

data Context = Context
  { token :: Token
  }
