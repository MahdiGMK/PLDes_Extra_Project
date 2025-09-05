<div dir='rtl'>

# پیاده سازی سیستم نوعی با متغیر های affine و relative

## قواعد به فرم SOS

برای هر متغیر در محتوی انواع، مقادیر
var_type, type, min_visit, max_visit
را نگهداری میکنیم. در نتیجه قواعد نوعی به شکل زیر مینویسیم :

$$
  [CONDITION] \Rightarrow \Gamma ⊢ e : (vt, T, mn, mx) \Rightarrow \Gamma'
$$

به طوری که $\Gamma$ محتوی انواع اولیه و
$\Gamma'$
محتوی انواع ثانویه است.

همینطور هروقت از TT استفاده شد یعنی صرفا نوع خود نوع عبارت را میگیریم.

برای برقرار کردن شروط affine و relative گزاره زیر را بررسی میکنیم :

$$
vt=affine \Rightarrow mx = mn \And mx <1
$$

به این شکل مطمئن میشویم یا دقیقا ۱ بار از این متغیر استفاده میشود و یا هیچگاه استفاده نمیشود.

$$
vt=relative \Rightarrow mn > 0
$$

به این شکل مطمئن میشویم که این متغیر حداقل ۱ بار استفاده شده است.

1. VarExpr

   $$
    [
     \Gamma(x) = (vt, T, mn, mx)
    ]
    $$ $$
     \Gamma ⊢ x : (vt, T, mn+1, mx+1) \Rightarrow \Gamma'
   $$

2. ConstExpr

$$
[true] \Rightarrow \Gamma ⊢ n : int \Rightarrow \Gamma
\hspace{1cm}
[true] \Rightarrow \Gamma ⊢ s : str \Rightarrow \Gamma
\hspace{1cm}
[true] \Rightarrow \Gamma ⊢ b : bool \Rightarrow \Gamma
$$

$$
[true] \Rightarrow \Gamma ⊢ l : list \Rightarrow \Gamma
\hspace{1cm}
[true] \Rightarrow \Gamma ⊢ r : Record \Rightarrow \Gamma
$$

3. FuncEvalExpr

$$
[ \forall_i \Gamma_i ⊢ e_i : TT_i \Rightarrow \Gamma_{i+1} \And f:(TT_1,...,TT_n \rightarrow T) ]
$$ $$
\Rightarrow
\Gamma_1 ⊢ f(e_1, ..., e_n) : T \Rightarrow \Gamma_{n+1}
$$

4. SumExpr

$$
[ \forall_i \Gamma_i ⊢ e_i : TT_i \Rightarrow \Gamma_{i+1} \And TT_1 = TT_2 ]
$$ $$
\Rightarrow
\Gamma_1 ⊢ e_1 + e_2 : TT_1 \Rightarrow \Gamma_2
$$

5. NegExpr

   $$
   [ \Gamma ⊢ e : bool \Rightarrow \Gamma' ]
   $$ $$
   \Rightarrow
   \Gamma ⊢ !e : bool \Rightarrow \Gamma'
   $$

6. IfElseExpr

   $$
   [ \Gamma_0 ⊢ e_c : bool \Rightarrow \Gamma_1 \And
   \Gamma_1 ⊢ e_t : TT \Rightarrow \Gamma_2 \And
   \Gamma_1 ⊢ e_f : TT \Rightarrow \Gamma_3
   ]
   $$ $$
   \Rightarrow
   \Gamma_0 ⊢ \text{ if } e_c \text{ then } e_t \text{ else }  e_f : TT \Rightarrow \Gamma4
   $$

   به طوری که

   $$
   \forall x:(vt,T,mn,mx) \in \Gamma_4
   , \Gamma_2(x) = (vt,T, mn0, mx0)
   , \Gamma_3(x) = (vt,T, mn1, mx1)
   $$

   $$
   \Rightarrow
   mn = min(mn0, mn1) ,
   mx = max(mx0, mx1)
   $$

7. RecExpr
   $$
   [ \forall_i \Gamma_i ⊢ e_i : TT_i \Rightarrow \Gamma_{i+1} \And R = TT_1 \times ... \times TT_n ]
   $$ $$
   \Rightarrow
   \Gamma_1 ⊢ R\{f_1 = e_1, ..., f_n = e_n\} : R \Rightarrow \Gamma_{n+1}
   $$
8. ListExpr
   $$
   [ \forall_i \Gamma_i ⊢ e_i : TT \Rightarrow \Gamma_{i+1}]
   $$ $$
   \Rightarrow
   \Gamma_1 ⊢ [e_1, ..., e_n] : List(TT) \Rightarrow \Gamma_{n+1}
   $$

## پیاده سازی

برای مشاهده نحوه پیاده سازی سیستم نوعی میتوانید به struct TypeContext
مراجعه کنید :

این ساختار شامل داده ساختار های مورد نیاز برای نگهداری توابع، متغیر ها و انواع و همینطور پیاده سازی
مکانیزم های relative و affine است.

در زیر به طور خلاصه توابع آنرا توضیح میدهیم :

</div>

```rust
pub fn begin_branch(&mut self)
```

<div dir='rtl'>

</div>

```rust
pub fn else_branch(&mut self)
```

<div dir='rtl'>

</div>

```rust
pub fn end_branch(&mut self)
```

<div dir='rtl'>

این توابع برای مطلع کردن محتوی از یک شاخه استفاده میشودن.
همینطور تابع end_branch مسئول پیاده سازی مکانیزم تجمیع شاخه هاست که در بالا توضیح داده شده بود.

</div>

```rust
pub fn resolve_var(&mut self, varname: &str) -> Result<TypeInfo, (EvalError, String)>
```

<div dir='rtl'>

این تابع معادل با استفاده کردن از یک متغیر است و mn و mx را نیز بروزرسانی میکند.

</div>

```rust
pub fn resolve_typename(&self, typename: &str) -> Result<TypeInfo, (EvalError, String)>
```

<div dir='rtl'>

این تابع اسم یک رکورد را به خود آن رکورد تبدیل میکند.

</div>

```rust
pub fn resolve_func(
        &self,
        funcname: &str,
    ) -> Result<(Vec<(VarType, String, TypeInfo)>, TypeInfo), (EvalError, String)>
```

<div dir='rtl'>

این تابع با گرفتن نام یک تابع، اطلاعات نوعی آنرا پیدا میکند.

</div>

```rust
pub fn define_var(
        &mut self,
        var_type: VarType,
        name: String,
        Tinfo: TypeInfo,
    ) -> Result<(), (EvalError, String)>
```

<div dir='rtl'>

</div>

```rust
pub fn define_func(
        &mut self,
        name: String,
        params: Vec<(VarType, String, TypeInfo)>,
        expr: &Expr,
    ) -> Result<(), (EvalError, String)>
```

<div dir='rtl'>

</div>

```rust
pub fn define_rec(
        &mut self,
        name: String,
        fields: Vec<(String, TypeInfo)>,
    ) -> Result<(), (EvalError, String)>
```

<div dir='rtl'>

با استفاده از این توابع میتوانیم متغیر ها، توابع و رکورد هارا برای سیستم نوع آشکار کنیم.

</div>

```rust
pub fn check_affine_relative(
        &self,
        vars: &Vec<(VarType, String, TypeInfo)>,
        func_visits: &HashMap<String, (u32, u32)>,
    ) -> Result<(), (EvalError, String)>
```

<div dir='rtl'>

این تابع مسئول چک کردن صحت affine یا relative بودن متغیر ها در یک تابع و یا در انتهای برنامه است.

## آزمون

برای آزمایش این سیستم نوعی، از پکیج serde و serde_json استفاده میکنیم تا نوع های تعریف شده در
کد را به جیسون تبدیل و جیسون را به انها پارس کنیم.
همینطور خود تست هارا به عنوان تست های زبان راسط مینویسیم و
مثال های مورد نیاز را در پوشه testing قرار میدهیم و از آنجا بارگیری میکنیم و اجرا میکنیم.

### simple0

دستور :
`cargo test test0 -- --nocapture`

json :

</div>

```json
[
  { "VarDef": ["Affine", "x", { "ConstExpr": { "IntVal": 0 } }] },
  {
    "VarDef": [
      "Affine",
      "lst",
      {
        "ConstExpr": {
          "ListVal": [{ "IntVal": 1 }, { "IntVal": 2 }, { "IntVal": 3 }]
        }
      }
    ]
  },
  {
    "Expr": {
      "SumExpr": [{ "VarExpr": "lst" }, { "ListExpr": [{ "VarExpr": "x" }] }]
    }
  }
]
```

<div dir='rtl'>

خروجی :

```
running 1 test
[OK]
test tests::test0 ... ok
```

همینطور که مشاهده میکنید، این مثال موردی ندارد و به درستی چک میشود.

مثال های بعدی را میتوانید از پوشه testing مشاهده کنید.

### sample1

دستور :
`cargo test test1 -- --nocapture`
خروجی :

```
running 1 test
[ERROR] NameDuplication : variable with name "x" already exists
test tests::test1 ... ok
```

### sample2

دستور :
`cargo test test2 -- --nocapture`
خروجی :

```
running 1 test
[ERROR] RelevantVarUnused : Relative variable "y" may remain unused
test tests::test2 ... ok
```

### sample3

دستور :
`cargo test test3 -- --nocapture`
خروجی :

```
running 1 test
[ERROR] AffineVarMoreThanOnce : Affine variable "x" may be used 2>1 times || 2<=usage<=2
test tests::test3 ... ok
```

### sample4

دستور :
`cargo test test4 -- --nocapture`
خروجی :

```
running 1 test
[ERROR] MismatchedType : Expected bool in if condition, found (VarExpr("lst")) :: List(Int)
test tests::test4 ... ok
```

### sample5

دستور :
`cargo test test5 -- --nocapture`
خروجی :

```
running 1 test
[ERROR] AffineVarNonDeterministic : Affine variable "x" usage is inconsistant
test tests::test5 ... ok
```

### sample6

دستور :
`cargo test test6 -- --nocapture`
خروجی :

```
running 1 test
[ERROR] RelevantVarUnused : Relative variable "y" may remain unused
test tests::test6 ... ok
```

### sample7

دستور :
`cargo test test7 -- --nocapture`
خروجی :

```
running 1 test
[ERROR] MismatchedType : Expected if to have matched types on the then and else sides, found (ConstExpr(IntVal(0))) :: Int != Boolean :: (ConstExpr(BoolVal(true)))
test tests::test7 ... ok
```

### sample8

دستور :
`cargo test test8 -- --nocapture`
خروجی :

```
running 1 test
[OK]
test tests::test8 ... ok
```

</div>
