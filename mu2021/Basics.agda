
-- Reading type signatures.

id' : (A : Set) → A → A
id' _ x = x

id : {A : Set} → A → A
id x = x

app : {A : Set} → {B : A → Set} → (f : (x : A) → B x) → (x : A) → B x
app f x = f x

-- Natural numbers.

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

infixl 20 _+_

_+_ : ℕ → ℕ → ℕ
zero    + m = m
(suc n) + m = suc (n + m)

infixl 40 _*_
_*_ : ℕ → ℕ → ℕ
zero   * n = zero
suc m  * n = n + (m * n)

-- Equality

infix 4 _≡_

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

cong : {A B : Set} → ∀ (f : A → B) {x y} → x ≡ y → f x ≡ f y
cong f refl = refl

sym : {A : Set} → {a b : A} → a ≡ b → b ≡ a
sym refl = refl

trans : {A : Set} → (a b c : A) → a ≡ b → b ≡ c → a ≡ c
trans a .a c refl b≡c = b≡c

-- Basic Properties.

+0 : ∀ {m} → m + zero ≡ m
+0 {zero}  = refl
+0 {suc m} = cong suc +0

+s : ∀ {m n} → m + suc n ≡ suc (m + n)
+s {zero}  {n} = refl
+s {suc m} {n} = cong suc +s

+assoc : ∀ m n k → (m + n) + k ≡ m + (n + k)
+assoc zero    n k = refl
+assoc (suc m) n k = cong suc (+assoc m n k)

+comm : ∀ m n → m + n ≡ n + m
+comm zero    n = sym +0
+comm (suc m) n = trans (suc (m + n)) (suc (n + m)) (n + suc m) (cong suc (+comm m n)) (sym +s)

-- Using `rewrite` (making quite a bit easier {^_^}).

{-# BUILTIN EQUALITY _≡_ #-}

*distrib+ : ∀ k m n → k * (m + n) ≡ k * m + k * n
*distrib+ k m n = {!!}
