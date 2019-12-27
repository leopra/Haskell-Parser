quadruple x = let twice_x = x+x
              in twice_x + twice_x

infinite n = letrec ns = cons n ns
              in ns

