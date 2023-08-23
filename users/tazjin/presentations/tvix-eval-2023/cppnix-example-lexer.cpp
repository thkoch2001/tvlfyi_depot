attrpath
  : attrpath '.' attr {
    $$ = $1; $1->push_back(AttrName(data->symbols.create($3)));
  }
  | attrpath '.' string_attr
    { $$ = $1;
      ExprString * str = dynamic_cast<ExprString *>($3);
      if (str) {
          $$->push_back(AttrName(data->symbols.create(str->s)));
          delete str;
      } else
          $$->push_back(AttrName($3));
    }
