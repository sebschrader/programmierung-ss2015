Vereinfache man
```
<Y><F><2><2><2>
```
mit 
```
<Y> = (λh.((λy.h(yy))(λy.h(yy))))
<F > = (λfxyz.
  <ite> (<iszero>(<pred> y))  
    (<mult>xz)  
    (<add><1> (f (<add>xz)(<pred>y)(<sub>xz))) 
) 
```

Zuerst löst man die Abstraktion des Fixpunktkombinators auf.
```
<Y> <F>  = (λh.((λy.h(yy))(λy.h(yy)))) <F>
=>β 
(λy.<F> (yy)) (λy.<F> (yy)) = t_F t_F
=>β
<Y>  (λy.<F> (yy))(λy.<F> (yy)) = <F>  (t_F t_F)
```

Nun kann man weiter den Term vereinfachen.
```
  <F >(t_F t_F) <2><2><2>= 
(λfxyz 
  <ite> (<iszero>(<pred> y)) 
    (<mult>xz) 
    (<add><1> (f (<add>xz)(<pred>y)(<sub>xz)))
)(t_F t_F) <2><2><2>
=>β
<ite>(<iszero>(<pred> <2>)) 
  (<mult><2><2>) 
  (<add><1>((t_F t_F) (<add><2><2>)(<pred><2>)(<sub><2><2>)))
=>β
<ite> (<iszero><1>) 
  (<mult><2><2>) 
  (<add><1> ((t_F t_F) (<add><2><2>)(<pred><2>)(<sub><2><2>)))
=>β
<ite> (<false>) 
  (<mult><2><2>) 
  (<add><1> ((t_F t_F) (<add><2><2>)(<pred><2>)(<sub><2><2>)))
=>β
(<add><1> ((t_F t_F) (<add><2><2>)(<pred><2>)(<sub><2><2>)))
=>β
(<add><1> (<F>(t_F) (<4>)(<1>)(<0>)))
=>β
(<add><1>
((λfxyz 
  <ite> (<iszero>(<pred> y)) 
    (<mult>xz) 
    (<add><1> (f (<add>xz)(<pred>y)(<sub>xz)))
)(t_F t_F) <4><1><0>)
=>β
<add><1>
(<ite> (<iszero>(<pred> <1>)) 
  (<mult><4><0>) 
  (<add><1> ((t_F t_F) (<add><4><0>)(<pred><1>)(<sub><4><0>)))
)
=>β
<add><1>
(<ite> (<iszero><0>)) 
  (<mult><4><0>) 
  (<add><1> ((t_F t_F) (<add><4><0>)(<pred><1>)(<sub><4><0>)))
)
=>β
<add><1>
(<ite> (<true>)) 
  (<mult><4><0>) 
  (<add><1> ((t_F t_F) (<add><4><0>)(<pred><1>)(<sub><4><0>)))
)
=>β
<add><1>(<mult><4><0>) 
=>β
<add><1><0>
=>β
<1>
```
