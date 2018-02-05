(*testのため、自作の関数の名前の先頭にmyを付けています*)
(*trigo_check.mlの関数にはtrigonometric.mlから変えた箇所がありますが、それはコアと同じ32bitの演算をするためです。*)
(*これにより、trigonometric.mlをコンパイルしてコアで実行したときと同じ計算結果が得られているはずです。*)
(*コアと同じ32bitの演算をするために、シミュレータ係の作ったfloat32.mlとfloat32.cを流用しています*)
open Int32
open Float32
open Printf

let pi = bits_of_float 3.1415927410125732421875

let reduction_2pi(x) =
  let p = mulf32 pi  (bits_of_float 2.0) in
  let rec ploop y =
    if ((float_of_bits x)>=(float_of_bits y)) then ploop (mulf32 (bits_of_float 2.0)  y) else y in 
  let rec ploop2 a p2 =
    if ((float_of_bits a)>=(float_of_bits p)) then (if (float_of_bits a)>=(float_of_bits p2) then ploop2 (subf32 a p2) (divf32 p2 (bits_of_float 2.0)) else ploop2 a (divf32 p2 (bits_of_float 2.0)))
    else a in
  ploop2 x (ploop p)

let kernel_sin a =
  let aa = mulf32 a a in
  mulf32 (addf32 ( mulf32 ( addf32 (mulf32 ( addf32 (mulf32 (bits_of_float(-0.00019587841)) aa) (bits_of_float 0.008332824)) aa) (bits_of_float (-0.16666668)) )  aa)  (bits_of_float 1.0) )  a

let kernel_cos a =
  let aa = mulf32 a a in
  addf32 (mulf32 (addf32 ( mulf32 (addf32(mulf32 (bits_of_float(-0.0013695068))  aa) (bits_of_float 0.04166368))  aa) (bits_of_float (-0.5))) aa) (bits_of_float 1.0)

let rec mysin x =
  let a = ((float_of_bits x)<0.0) in
  let b = if a then subf32 zero x else x in
  let c = reduction_2pi b in
  let e = ((float_of_bits c)>=(float_of_bits pi)) in
  let d = (if e then (subf32 c pi) else c) in
  let f = (if e then (not a) else a) in
  let g = (if (float_of_bits d>=float_of_bits (divf32 pi (bits_of_float 2.0))) then subf32 pi d else d ) in
  let h = (if (float_of_bits g<=float_of_bits ( divf32 pi (bits_of_float 4.0))) then kernel_sin(g) else kernel_cos(subf32 (divf32 pi (bits_of_float 2.0))  g)) in
  (if f then subf32 zero h else h) 

let rec mycos x =
  let a = ((float_of_bits x)<0.0) in
  let b = if a then subf32 zero x else x in
  let c = reduction_2pi b in
  let e = ((float_of_bits c)>=(float_of_bits pi)) in
  let d = (if e then (subf32 c pi) else c) in
  let f = (float_of_bits d>=float_of_bits (divf32 pi (bits_of_float 2.0))) in
  let g = (if f then subf32 pi d else d) in
  let g2 = (if f then not e else e) in
  let h = (if (float_of_bits g<=float_of_bits ( divf32 pi (bits_of_float 4.0))) then kernel_cos(g) else kernel_sin(subf32 (divf32 pi (bits_of_float 2.0))  g)) in
  (if g2 then subf32 zero h else h) 

let  kernel_atan a =
  let aa = mulf32 a a in
  mulf32 a (addf32 (bits_of_float 1.0)  (mulf32 aa  (addf32 (bits_of_float (-0.3333333)) (mulf32 aa (addf32 (bits_of_float 0.2) (mulf32 aa (addf32 (bits_of_float (-0.142857142)) (mulf32 aa  (addf32 (bits_of_float 0.111111104) (mulf32 aa (addf32 (bits_of_float (-0.08976446))  (mulf32 aa  (bits_of_float 0.060035485)))))))))))))

let myatan x =
   let a = (float_of_bits x<0.0) in
   let b = if a then subf32 zero x else x in
   let c = if float_of_bits b < 0.4375 then kernel_atan b 
   else (if float_of_bits b <2.4375 then addf32 (divf32 pi (bits_of_float 4.0)) (kernel_atan(divf32 (subf32 b (bits_of_float 1.0)) (addf32 b (bits_of_float 1.0))))
   else subf32 (divf32 pi (bits_of_float 2.0))  (kernel_atan(divf32 (bits_of_float 1.0) b))) in
   if a then subf32 zero c else c


(*check*)

(*sinは[0.1)の単精度浮動小数点数に対するチェック*)
let mysincheck =
  Printf.printf "\n sinのチェック \n";
  let r = Array.make 20 0 in  
  let a = ref zero in
  let d = (float_of_bits (mysin(!a))) -. sin(float_of_bits !a)  in
  if d<0.0 && -.(float_of_bits(of_int 8388608)) < d
    then r.(9) <- r.(9) + 1
    else (
    if d>=0.0 && (float_of_bits(of_int 8388608)) > d 
    then r.(10) <- r.(10) + 1
    else
    Printf.printf "0.0が条件を満たさない%e\n" d
    );
  a := of_int (8388608);
  for i = 1 to 126 do (*指数部*)
    for j = 0 to 8388607 do(*仮数部*)
      let sina = sin(float_of_bits !a) in
      let ulp = (Int64.float_of_bits(Int64.mul (Int64.of_int (8388608*536870912)) (Int64.div (Int64.bits_of_float sina) (Int64.of_int (8388608*536870912)))))/.8388608.0 in
      let c = int_of_float (floor ( ( (float_of_bits (mysin(!a))) -. sina ) /. ulp ))in
      r.(c+10) <- r.(c+10) + 1;
      a := succ !a;   
    done;
  done;
  for i = 0 to 19 do
    printf "%3d ulp以上 %3d ulp未満 : %10d\n" (i-10)  (i-9) r.(i);
  done


(*cosは[0.1)の単精度浮動小数点数に対するチェック*)
let mycoscheck =
  Printf.printf "\n cosのチェック \n";
  let r = Array.make 20 0 in  
  let a = ref zero in
  let d = (float_of_bits (mycos(!a))) -. cos(float_of_bits !a)  in
  if d<0.0 && -.(float_of_bits(of_int 8388608)) < d
    then r.(9) <- r.(9) + 1
    else (
    if d>=0.0 && (float_of_bits(of_int 8388608)) > d 
    then r.(10) <- r.(10) + 1
    else
    Printf.printf "0.0が条件を満たさない%e\n" d
    );
  a := of_int (8388608);
  for i = 1 to 126 do (*指数部*)
    for j = 0 to 8388607 do(*仮数部*)
      let cosa = cos(float_of_bits !a) in
      let ulp = (Int64.float_of_bits(Int64.mul (Int64.of_int (8388608*536870912)) (Int64.div (Int64.bits_of_float cosa) (Int64.of_int (8388608*536870912)))))/.8388608.0 in
      let c = int_of_float (floor ( ( (float_of_bits (mycos(!a))) -. cosa ) /. ulp ))in
      r.(c+10) <- r.(c+10) + 1;
      a := succ !a;   
    done;
  done;
  for i = 0 to 19 do
    printf "%3d ulp以上 %3d ulp未満 : %10d\n" (i-10)  (i-9) r.(i);
  done

(*atanは0以上の単精度浮動小数点数に対する全数チェック*)
let myatancheck =
  Printf.printf "\n atanのチェック \n";
  let r = Array.make 20 0 in  
  let a = ref zero in
  let d = (float_of_bits (myatan(!a))) -. atan(float_of_bits !a)  in
  if d<0.0 && -.(float_of_bits(of_int 8388608)) < d
    then r.(9) <- r.(9) + 1
    else (
    if d>=0.0 && (float_of_bits(of_int 8388608)) > d 
    then r.(10) <- r.(10) + 1
    else
    Printf.printf "0.0が条件を満たさない%e\n" d
    );
  a := of_int 8388608;
  for i = 1 to 254 do (*指数部*)
    for j = 0 to 8388607 do(*仮数部*)
      let atana = atan(float_of_bits !a) in
      let ulp = (Int64.float_of_bits(Int64.mul (Int64.of_int (8388608*536870912)) (Int64.div (Int64.bits_of_float atana) (Int64.of_int (8388608*536870912)))))/.8388608.0 in
      let c = int_of_float (floor ( ( (float_of_bits (myatan(!a))) -. atana ) /. ulp ))in
      r.(c+10) <- r.(c+10) + 1;
      a := succ !a;   
    done;
  done;
  for i = 0 to 19 do
    printf "%3d ulp以上 %3d ulp未満 : %10d\n" (i-10)  (i-9) r.(i);
  done





