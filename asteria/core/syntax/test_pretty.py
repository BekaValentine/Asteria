import asteria.core.syntax.concrete as concrete
import asteria.core.syntax.abstract as abstract


src = '''

    x : M$T()
      = \{a} -> f x
      ;;

'''

cst = concrete.parse('unknown', src)

print(cst.pretty())

mod = abstract.Module_from_cst(cst)

print(mod.pretty())
