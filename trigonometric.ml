(*fabsは使用せずにif文で絶対値を求めています*)
(*このプログラムをmin-rt.mlと接続する具体的な方法は分かりません*)
(*このプログラムではbool値true,falseを使用しています*)
(*min-rt.mlではtrue,falseが1,0と定義されています。理由は分かりません(minCamlにはbool値があるのに)*)
(*どうやってpiの即値(32bit)をコンパイル、アセンブルしてメモリに格納するのかは分かりません*)
(*裸のlet文の羅列をコンパイラが読めるのかは分かりません*)
(*これおよびminiMLRuntime.mlがどのようにコンパイルされるのかは分かりません*)
(*数式上、float_specを満たすようになっているはずです。動作確認はcheckディレクトリを見てください*)
(*piの値は単精度浮動小数点数の中でなるべく円周率に近いもの(0x40490fdb)の10進数表記です。*)
let pi = 3.1415927410125732421875

let reduction_2pi(x) =
  let p = pi *. 2.0 in
  let rec ploop y =
    if (x>=y) then ploop (2.0 *. y) else y in 
  let rec ploop2 a p2 =
    if (a>=p) then (if a>=p2 then ploop2 (a-.p2) (p2/.2.0) else ploop2 a (p2/.2.0))
    else a in
  ploop2 x (ploop p)

let kernel_sin a =
  let aa = a*.a in
  ((((-0.00019587841) *. aa +. 0.008332824) *. aa +. (-0.16666668)) *. aa +. 1.0) *. a

let kernel_cos a =
  let aa = a*.a in
  (((-0.0013695068) *. aa +. 0.04166368) *. aa +. (-0.5)) *. aa +. 1.0

let rec sin x =
  let a = (x<0.0) in
  let b = if a then -.x else x in
  let c = reduction_2pi b in
  let e = (c>=pi) in
  let d = (if e then (c-.pi) else c) in
  let f = (if e then (not a) else a) in
  let g = (if (d>=pi/.2.0) then pi -. d else d ) in
  let h = (if (g<=pi/.4.0) then kernel_sin(g) else kernel_cos(pi/.2.0 -. g)) in
  (if f then -.h else h) 

let rec cos x =
  let a = (x<0.0) in
  let b = if a then -.x else x in
  let c = reduction_2pi b in
  let e = (c>=pi) in
  let d = (if e then (c-.pi) else c) in
  let f = (d>=pi/.2.0) in
  let g = (if f then pi -. d else d) in
  let g2 = (if f then not e else e) in
  let h = (if (g<=pi/.4.0) then kernel_cos(g) else kernel_sin(pi/.2.0 -. g)) in
  (if g2 then -.h else h) 

let  kernel_atan a =
  let aa = a*.a in
  a *. (1.0 +. aa *. (-0.3333333 +. aa *. (0.2+. aa *.(-0.142857142 +. aa *. (0.111111104 +. aa *.(-0.08976446 +. aa *. (0.060035485)))))))

let atan x =
   let a = (x<0.0) in
   let b = if a then -.x else x in
   let c = if b < 0.4375 then kernel_atan b 
   else (if b <2.4375 then pi/.4.0 +. kernel_atan((b-.1.0)/.(b+.1.0))
   else pi/.2.0 -. kernel_atan(1.0/.b)) in
   if a then -.c else c
