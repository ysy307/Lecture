# 生物生産工学Ⅰ

## コンパイル
Intel oneAPIとGNU makeでコンパイルできます．
```
make test01
```

適当なところでクローンして使ってください．
```
git clone https://github.com/ysy307/Lecture.git
```


## Lecture_01.f90
### 問1.1   
$` x = 0.00002311 `$のとき$` \dfrac{1}{\sqrt{1-x}}-\frac{1}{\sqrt{1+x}} `$の値をコンピュータにより，単精度で有効数字4桁まで計算せよ．

実行結果
```
 single   :  2.3126602E-05
 double   :  2.311000000765340E-005
 quad     :  2.311000000771400389761936948062395E-0005
 Diff(d-s): -1.6601916E-08
 Diff(q-d):  6.060690144193970E-017
```

### 問1.2
$` x=0.003 `$のとき，次式を単精度により数値計算し，その結果を比較検討せよ．
```math
\begin{equation*}
\displaystyle
  s_1=\frac{1}{\sqrt{1-\sin^2x}}-1,\quad s_2=\frac{\sin^2 x}{(1+\cos x)(\cos x)}
\end{equation*} 
```

実行結果

```
 s1-single:  4.5299530E-06
 s2-single:  4.5000174E-06
 s1-double:  4.500016875130797E-006
 s2-double:  4.500016875061763E-006
 s1-quad  :  4.500016875061762725372473882625120E-0006
 s2-quad  :  4.500016875061762725372473882663843E-0006
```

### 問1.4
無限級数
```math
\begin{equation*}
\displaystyle
  \frac{\pi}{4}=1-\frac{1}{3}+\frac{1}{5}-\cdots=\sum_{k=0}^{\infty}\frac{(-1)^k}{2k+1}
\end{equation*}
```
をエイトケン加速のプログラムにより求めよ．ただし，講義で示した数列$` S_n^{(k)} `$の$` n=9 `$まで求めよ．

実行結果: $` S_9^{(9)} `$
```
0.78539875048231589
```

## Lecture_02.f90
### 問2.2
次の3点を通る２次関数の，$` x=1.16 `$における値を求めよ．
| $i$ |$x_i$  | $y_i$   |
|:---:|:-----:|:-------:|
| 1   | 1.1   | 0.89121 |
| 2   | 1.2   | 0.03204 |
| 3   | 1.3   | 0.96356 |

実行結果

```math
y_{obj} = a - bx- cx^2
```

```
a (LU) =  128.52762000000
b (LU) = -214.52105000000
c (LU) =   89.53450000000
a      =  128.52762000000
b      = -214.52105000000
c      =   89.53450000000
y_obj  =    0.16082542701

```
### 問2.3
$\log 9.2$ をラグランジュの内挿を用いて計算せよ．ただし，$f(x)=\log x$とし，補間点として下の 4 点をとるものとする．

|$x$  | $f(x)$  |
|:---:|:-------:|
|9.0  | 2.19722 |
|9.5  | 2.25129 |
|10.0 | 2.30259 |
|11.0 | 2.39790 |

実行結果
```
L (9.2) = 2.219196720000
ln(9.2) = 2.219203484055
```
### 問2.5
```math
\begin{equation*}
\displaystyle
  f(x)=\frac{1}{1+25x^2}
\end{equation*}
```

を区間$x=[-1,1]$で補間点を等間隔$x_i= - 1 + 2i / N \quad(i=0,1,2,\ldots ,N)$にとり，$N= 12 , 24 , 36 , 48$ について 1 次，2 次，3 次，4 次のラグランジュ補間式および 3 次スプライン関数で補間し，それぞれの精度を調べよ．誤差の指標として，それそれの補間法による最適補間関数$f_m(x)$と元の関数$(x)$差$f_m(x)-f(x)$の$L_2$ノルムの 2 乗を計算せよ．

実行結果
```
Enter the number of division, N:
12
Enter Size(0:small, 1:medium, 2:big)
1
Size = 248832
norm =  1.15646901E+02 6.61263056E+00 2.02399847E-02 4.51972866E-03 8.19846850E-01
MSE  =  4.64758959E-04 2.65746791E-05 8.13399590E-08 1.81637758E-08 3.29478061E-06
```
normおよびMSEは左から1,2,3,4次ラグランジュ補間，3次スプライン補間である．

## Lecture_03.f90
### 問3.1
```math
\begin{equation*}
\displaystyle
I=\int_{0}^{\frac{\pi}{2}}\sin x~\mathrm{d}x
\end{equation*}
```
を $` N = 8 `$ として台形公式により積分値を求めよ．

実行結果

解析解：$` 1 `$
```
The integral of sin(x) from 0 to pi/2 is   0.996785171886170
Error is  3.2148281E-03
```

### 問3.2
```math
\begin{equation*}
\displaystyle
I=\int_{0}^{\frac{\pi}{2}}\sin x~\mathrm{d}x
\end{equation*}
```
を $` N = 8 `$ としてシンプソン則により積分値を求めよ．

実行結果

解析解：$` 1 `$
```
The integral of sin(x) from 0 to pi/2 is    1.00000829552397
Error is -8.2955240E-06
```

### 問3.3
```math
\begin{equation*}
\displaystyle
S=\int_{0}^{1}\frac{4}{1+x^2}~\mathrm{d}x
\end{equation*}
```
をシンプソン則で計算し，分割数$` N `$と精度の関係を検討せよ．

計算結果

解析解：$` \pi ` $
```
Enter even division number (N):
4
The integral is    3.14156862745098
Error is -2.4026139E-05

Enter even division number (N):
8
The integral is    3.14159250245871
Error is -1.5113109E-07

Enter even division number (N):
64
The integral is    3.14159265358922
Error is -5.7731597E-13

Enter even division number (N):
1048576
The integral is    3.14159265358977
Error is -2.4868996E-14
```

### 問3.4
次の定積分の値を，台形公式およびシンプソン則を用いて数値計算せよ．ただし，それぞれの公式において，分割数を$` N= 8 , 16 , 32 , 64 `$ の場合について計算せよ．

```math
\begin{equation*}
\displaystyle
S=\int_{0}^{2}x^5~\mathrm{d}x
\end{equation*}
```

計算結果

解析解：$` \dfrac{32}{3} `$
```
Number of Division: 8
The integral is    10.6718750000000
Error is  5.2083333E-03
Number of Division: 16
The integral is    10.6669921875000
Error is  3.2552083E-04
Number of Division: 32
The integral is    10.6666870117188
Error is  2.0345052E-05
Number of Division: 64
The integral is    10.6666679382324
Error is  1.2715658E-06
```

### 問3.5
問 3.4 における定積分の値をガウス・ルジャンドル積分公式$` (N=3) `$を用いて求めよ．

計算結果

解析解：$` \dfrac{32}{3} `$
```
Number of Division: 3
The integral is    10.6666666666667
Error is  3.5527137E-15
```

### 問3.6
下記の式に台形公式，シンプソン公式，ガウス・ルジャンドル積分公式を適用し，数値計算したときの積分$I$に含まれる誤差$` [I-\pi/2] `$ の値を表形式で示せ．ただし，$` N = 8 , 12 , 16 , 20 `$  とせよ．
```math
\begin{equation*}
\displaystyle
    f(x) =  \sqrt{1-x^2}
\end{equation*}
```
```math
\begin{equation*}
\displaystyle
    I=\int_{-1}^{1} f(x) ~\mathrm{d}x = \int_{-1}^{1}\sqrt{1-x^2} ~\mathrm{d}x = \dfrac{\pi}{2}
\end{equation*}
```

実行結果

```
Trapezoidal| Number of Division: 8
The integral is    1.49785453405122     
Error is  7.2941793E-02
Simpson| Number of Division: 8
The integral is    1.54179757747348     
Error is  2.8998749E-02
Gaussian_Quadrature| Number of Division: 1
The integral is    2.00000000000000     
Error is  4.2920367E-01

Trapezoidal| Number of Division: 12
The integral is    1.53099151114742     
Error is  3.9804816E-02
Simpson| Number of Division: 12
The integral is    1.55506311840056     
Error is  1.5733208E-02
Gaussian_Quadrature| Number of Division: 2
The integral is    1.63299316185545     
Error is  6.2196835E-02

Trapezoidal| Number of Division: 16
The integral is    1.54490957217859     
Error is  2.5886755E-02
Simpson| Number of Division: 16
The integral is    1.56059458488771     
Error is  1.0201742E-02
Gaussian_Quadrature| Number of Division: 3
The integral is    1.59161725781520     
Error is  2.0820931E-02

Trapezoidal| Number of Division: 20
The integral is    1.55225916312416     
Error is  1.8537164E-02
Simpson| Number of Division: 20
The integral is    1.56350407935162     
Error is  7.2922474E-03
Gaussian_Quadrature| Number of Division: 4
The integral is    1.58027752770007     
Error is  9.4812009E-03
```

## Lecture_04.f90
### 問4.1

$` f(x)=x-\cos x `$ を2分法，1次元ニュートン法でそれそれで計算せよ．

計算結果

$` f'(x)=1+\sin x `$
```
Binary search       :  0.7390851332
Newton Method       :  0.7390851332
Convergence delta   :  1.0E-10
Convergence epsilon :  5.0E-06
```

### 問4.2
```math
\begin{equation*}
  f(x)=x^3+x-1=0
\end{equation*}
```
に対して1次元ニュートン法を適用せよ．

計算結果

$f'(x)=3x^2+1$
```
Newton Method       :  0.6823278038
Convergence delta   :  1.0E-10
```
### 問4.3
ニュートン法により次の方程式の実数解を求めよ．
```math
\begin{equation*}
  \sin (x) = \dfrac{1}{2}x
\end{equation*}
```

計算結果

```
Initial value       :  2.0000000000
Newton Method       :  1.8954942670
Convergence delta   :  1.0E-10
Initial value       :  0.5000000000
Newton Method       :  0.0000000000
Convergence delta   :  1.0E-10
Initial value       : -2.0000000000
Newton Method       : -1.8954942670
Convergence delta   :  1.0E-10
```

### 問4.4
2分法により次の方程式の実根を有効数字6桁まで求めよ．なお，初期値として $` a^{(0)}=-3.0 `$，$` b^{(0)}=0 `$ とし，判定条件式$` \left|f(c^{(k+1)})\right|<\delta `$，$` \left|a^{(k)}-b^{(k)}\right|<\varepsilon `$ で $` \delta=10^{-10} `$ および $` \varepsilon=5.0\times 10^{-5} `$ とせよ．
```math
\begin{equation*}
  f(x)=x^3 + 6x^2 + 21x + 32 = 0
\end{equation*}
```

計算結果

実数解 $` -2+ \sqrt[3]{3} - 3^{2/3} `$
```
Binary search       : -2.6378342527
Convergence delta   :  1.0E-10
Convergence epsilon :  5.0E-06
```

### 問4.5

ニュートン法により次の方程式の実根を有効数字6桁まで求めよ．なお，初期値として $`  a^{(0)}=0 `$ を用い，判定条件には $` \delta=10^{-10} `$ を用いよ．
```math
\begin{equation*}
  f(x)=x^3 + 6x^2 + 21x + 32 = 0
\end{equation*}
```

計算結果

実数解 $` -2+ \sqrt[3]{3} - 3^{2/3} `$
```
Newton Method       : -2.6378342527
Convergence delta   :  1.0E-10
```
### 問4.6
2次元ニュートン法により次の連立方程式の根を有効数字6桁まで求めよ．ただし，初期条件を $` (x^{(0)}, y^{(0)})=(-1,-2) `$　とし，収束条件 $` (f(x^{(k+1)}, y^{(k+1)}))^2 + (g(x^{(k+1)}, y^{(k+1)}))^2 < \delta^2 `$ においては $` \delta=10^{-10} `$を用いよ．
```math
\begin{equation*}
  \left\{ \,
  \begin{aligned}
      f(x,y) &= x^3 - 3 x y^2 + 6 x^2 - 6 y^2 + 21 x +32 = 0\\
      g(x,y) &= 3 x^2 y - y^3 + 12 x y + 21 y=0
    \end{aligned}
  \right.
\end{equation*}
```

計算結果

$` x\approx -2.6378, y = 0            `$<br>
$` x\approx -1.6811, y\approx -3.0504 `$<br>
$` x\approx -1.6811, y\approx  3.0504 `$<br>

```
Initial value     : -1.0000000000 -2.0000000000
Newton Method     : -1.6810828736 -3.0504301992
Convergence delta :  1.0E-10
```

### 問4.7
1次元ニュートン法により問 4.5 で示した方程式の複素根を有効数字6桁まで求めよ．なお，初期値を $` z= -1-3i `$ とし，判定条件において $` \delta = 10^{-10} `$ を用いよ．


計算結果

問4.6の初期値を変えればよい
```
Initial value     : -1.0000000000 -3.0000000000
Newton Method    z= -1.6810828736-3.0504301992i
Convergence delta :  1.0E-10
```