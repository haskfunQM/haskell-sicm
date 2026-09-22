# haskell-sicm

**haskell-sicm** is a small symbolic-computation and classical-mechanics library written in Haskell, inspired by *Structure and Interpretation of Classical Mechanics* (SICM) by Gerald Jay Sussman and Jack Wisdom.

The goal is not to build another giant computer algebra system. The goal is to make classical mechanics **executable, inspectable, and readable**:

```haskell
print $
    prove
        ( xdot ** 2 + ydot ** 2
            ===
          rdot ** 2 + r ** 2 * thetadot ** 2
        )
```

and

```haskell
equation =
    eulerLagrange state lagrangian
```

should look close to the mathematics they represent.

haskell-sicm follows a **Haskell-first, SICM-inspired** design: ordinary Haskell functions, algebraic data types, type classes, and small modules are preferred over recreating the entire `scmutils` environment.

---

## 1. Why SICM?

*Structure and Interpretation of Classical Mechanics* treats mathematics as something that can be **run**.

Instead of using a program only to calculate numbers after the mathematics has already been derived by hand, SICM represents mathematical objects directly as procedures and data:

- a path is a function of time;
- a Lagrangian is a function of a local mechanical state;
- differentiation is an operator on expressions or functions;
- generalized coordinates are structured values;
- Euler-Lagrange equations can be generated from the Lagrangian itself;
- identities can be checked symbolically.

This changes the role of the program. The code is not merely an implementation *after* the derivation: the code can participate in the derivation.

For example, a path

$$
q:t\mapsto q(t)
$$

is represented in Haskell as an ordinary function:

```haskell
type Path =
    Expr -> Structure Expr
```

A Lagrangian

$$
L=L(t,q,\dot q)
$$

is also an ordinary function:

```haskell
type Lagrangian =
    Local Expr -> Expr
```

Then the action

$$
S[q]
=
\int_{t_0}^{t_1}
L\left(t,q(t),\dot q(t)\right)\,dt
$$

can itself be implemented as a function whose **input is a path**.

That is the central idea haskell-sicm takes from SICM: mathematical objects should be manipulable program objects, while the resulting code should remain recognizable as mathematics.

---

## 2. Design goals

haskell-sicm currently focuses on five principles:

1. **Readable symbolic mathematics.** Public code should resemble the equations as closely as Haskell reasonably allows.
2. **Small symbolic core.** We implement only the symbolic machinery needed to support mechanics clearly.
3. **Explainable proofs.** `prove (lhs === rhs)` should not only decide simple identities but also retain useful proof steps.
4. **Structured mechanics.** Generalized coordinates are not forced into flat lists; nested `up` and `down` structures preserve mathematical roles.
5. **Reuse the Haskell ecosystem.** Numerical linear algebra, optimization, ODE solvers, and similar mature functionality should eventually be delegated to existing Haskell packages instead of being reimplemented unnecessarily.

haskell-sicm is therefore **not intended to become a full Mathematica/Maple replacement**. It is a compact executable mechanics system.

---

## 3. Building the project

A recent development build has been tested with:

```text
GHC 9.10.3
Cabal
```

From the project root:

```powershell
cabal build
cabal test
```

To run an example directly:

```powershell
cabal exec runghc -- -isrc examples/KeplerInverseSquare.hs
```

The same form can be used for the other standalone example files:

```powershell
cabal exec runghc -- -isrc examples/StationaryAction.hs
```

---

# 4. Symbolic computation

The symbolic layer is deliberately divided into small modules. Each module has one clear responsibility.

A simplified view is:

```text
SICM.Core.Expr
      |
      +--------------------+
      |                    |
      v                    v
Rewrite/Simplify     Differentiate/Integrate
      |
      +----------+
      |          |
      v          v
 Polynomial   Identity
      \          /
       \        /
        v      v
          Proof
```

## 4.1 `SICM.Core.Expr`

`Expr` is the symbolic expression tree.

The core representation is:

```haskell
data Expr
    = Number Rational
    | Symbol Name
    | Add [Expr]
    | Mul [Expr]
    | Pow Expr Expr
    | Apply Name [Expr]
    deriving (Eq, Ord)
```

Examples:

```haskell
x = symbol "x"
y = symbol "y"

f = x ** 2 + 2 * x * y + y ** 2
```

A symbolic function application such as

```haskell
sin x
```

is represented by an `Apply` node.

`Expr` implements standard numeric type classes, allowing ordinary Haskell notation to construct symbolic expressions:

```haskell
x + y
x * y
x ** 2
sin x
cos x
exp x
log x
```

Exact constants use `Rational`, so expressions such as `1 / 2` can remain exact rather than becoming floating-point approximations.

---

## 4.2 `SICM.Symbolic.Rewrite`

The rewrite layer provides named transformation rules and repeated rewriting.

Conceptually a rule is:

```text
pattern / condition
        |
        v
replacement
```

Typical simplification rules include:

```text
x + 0  -> x
x * 1  -> x
x * 0  -> 0
```

Rewrite rules are kept separate from higher-level proof logic so that the same machinery can be reused by the simplifier and identity system.

---

## 4.3 `SICM.Symbolic.Simplify`

`Simplify` performs inexpensive canonical reductions such as:

- flattening nested sums and products;
- combining numeric coefficients;
- removing additive and multiplicative identities;
- collecting repeated factors;
- collecting like terms;
- simplifying numeric powers.

Examples:

```haskell
simplify (x * 1)
-- x

simplify (x + x)
-- 2*x

simplify (2 * x ** 2 + x ** 2)
-- 3*x^2
```

The simplifier is intentionally conservative. Expansion rules that can make an expression larger are not blindly applied as ordinary simplifications.

---

## 4.4 `SICM.Symbolic.Polynomial`

The polynomial engine converts suitable expressions into a canonical multivariate polynomial representation.

This makes identities such as

$$
(x+y)^2=x^2+2xy+y^2
$$

decidable by normalization rather than by guessing rewrite sequences.

Typical public operations include:

```haskell
polynomialNormalForm
polynomialEquivalent
isPolynomial
```

The prover also has an algebraic mode in which expressions such as

```haskell
sin theta
cos theta
```

can be treated as **opaque algebraic atoms**.

This does **not** claim that `sin theta` is a polynomial in `theta`. It only means that an expression such as

$$
(a\sin\theta+b\cos\theta)^2
$$

can be expanded algebraically while the trigonometric subexpressions remain indivisible objects.

This capability is important in coordinate transformations.

---

## 4.5 `SICM.Symbolic.Identity`

The identity layer stores common mathematical knowledge used by the prover.

The current identity set includes, among others,

$$
\sin^2x+\cos^2x=1,
$$

$$
\sin(-x)=-\sin x,
\qquad
\cos(-x)=\cos x,
$$

the sine and cosine addition formulas, double-angle formulas, exact common-angle values, and simple exponential/logarithmic identities.

Expansion identities and reduction identities are kept conceptually distinct. For example,

$$
\sin(a+b)
=
\sin a\cos b+\cos a\sin b
$$

is useful during a proof, but automatically expanding every occurrence of `sin(a+b)` would not always make an expression simpler.

`Identity` is therefore primarily **prover knowledge**, rather than a collection of unconditional global simplifications.

---

## 4.6 `SICM.Symbolic.Differentiate`

Symbolic differentiation supports ordinary algebraic and elementary-function expressions.

For example:

```haskell
diff x (3 * x ** 2 + 5 * x + 2)
```

produces the equivalent of

$$
6x+5.
$$

The differentiator also has a trace interface for showing important mathematical steps rather than every internal rewrite.

A deliberately compact trace can show, for example:

```text
product rule
simplify
```

without exposing the entire execution log of the symbolic engine.

---

## 4.7 `SICM.Symbolic.Integrate`

The current integrator is intentionally small. It handles a useful subset including:

- constants;
- powers;
- `1/x`;
- elementary `sin`, `cos`, and `exp`;
- simple substitutions;
- selected integration-by-parts cases;
- polynomial expansion before integration.

Examples already supported include forms equivalent to

$$
\int x\,dx,
\qquad
\int x^{-1}\,dx,
\qquad
\int x\cos(x^2)\,dx,
\qquad
\int x\sin x\,dx.
$$

It is not intended to become a complete symbolic integration system.

---

## 4.8 `SICM.Symbolic.Function`

Ordinary Haskell functions are used whenever possible instead of introducing a large custom function hierarchy.

For an unknown literal symbolic function, the package provides:

```haskell
literalFunction
```

For example:

```haskell
u x =
    literalFunction "u" x
```

Most mechanics functions, however, can simply be ordinary Haskell functions.

---

## 4.9 `SICM.Symbolic.Proof`

The proof interface is one of the central public interfaces of haskell-sicm.

An equation is written as:

```haskell
lhs === rhs
```

and submitted to:

```haskell
prove
```

For example:

```haskell
print $
    prove
        ((x + y) ** 2 === x ** 2 + 2 * x * y + y ** 2)
```

or, after the addition of trigonometric identity knowledge:

```haskell
print $
    prove
        (sin x ** 2 + cos x ** 2 === 1)
```

A proof has a status such as:

```haskell
Proven
Unresolved
```

and may contain a residual expression and explanatory steps.

The prover currently combines several strategies:

```text
equation
   |
   +--> ordinary simplification
   |
   +--> polynomial canonicalization
   |
   +--> identity expansion/reduction
   |
   +--> algebraic normalization
   |
   +--> residual check
```

The important design goal is that the user should normally write:

```haskell
prove (lhs === rhs)
```

instead of needing to decide whether a particular identity should be solved using polynomial normalization, trigonometric rewriting, or another internal method.

This is an **equational symbolic prover**, not a formal theorem prover comparable to Lean or Coq.

---

# 5. Classical mechanics

The mechanics layer builds on the symbolic layer.

Its current organization is approximately:

```text
SICM.Math.Structure
        |
        v
SICM.Mechanics.Local
        |
        +----------------------+
        |                      |
        v                      v
SICM.Mechanics.Lagrange   SICM.Mechanics.Action
```

---

## 5.1 `SICM.Math.Structure`

SICM uses structured mathematical objects rather than flattening every quantity into an ordinary list.

haskell-sicm represents them as:

```haskell
data Structure a
    = Scalar a
    | Upward [Structure a]
    | Downward [Structure a]
```

Constructors are exposed through helpers such as:

```haskell
scalar
up
down
```

For polar coordinates:

```haskell
q =
    up
        [ scalar r
        , scalar theta
        ]
```

For their velocities:

```haskell
v =
    up
        [ scalar rdot
        , scalar thetadot
        ]
```

A useful elementary interpretation is:

- `up(...)` behaves like a coordinate/change-like column structure;
- `down(...)` behaves like a derivative/measurement-like row structure.

For example, differentiating a scalar with respect to an `up` coordinate structure naturally produces a `down` structure.

`Structure` also supports:

```haskell
dual
sameShape
zipWithStructure
leaves
```

`leaves` extracts all scalar components into an ordinary Haskell list. It is useful as a low-level bridge, but it intentionally discards the `up`/`down` structure information.

---

## 5.2 `SICM.Mechanics.Local`

A local mechanical state contains time, generalized coordinates, velocity, and optionally higher time derivatives.

Conceptually:

```haskell
data Local a =
    Local
        a
        (Structure a)      -- q
        (Structure a)      -- Dq
        [Structure a]      -- D^2q, D^3q, ...
```

The simple constructor:

```haskell
local t q v
```

represents

$$
(t,q,\dot q).
$$

For higher derivatives:

```haskell
localN t q v [acceleration]
```

can represent

$$
(t,q,\dot q,\ddot q).
$$

Accessors include:

```haskell
time
coordinates
velocity
acceleration
derivative
```

A path can be lifted into a local state using:

```haskell
gamma
gammaN
```

Mathematically,

$$
\Gamma[q](t)
=
(t,q(t),Dq(t),D^2q(t),\ldots).
$$

This is an important SICM idea: instead of manually maintaining a table such as

```text
q       -> qdot
qdot    -> qddot
```

the local state and `gamma` represent the derivative chain structurally.

---

## 5.3 Total time derivative `dt`

For a local expression

$$
F(t,q,\dot q,\ddot q,\ldots),
$$

the total derivative is

$$
D_tF
=
\frac{\partial F}{\partial t}
+
\frac{\partial F}{\partial q}\dot q
+
\frac{\partial F}{\partial\dot q}\ddot q
+\cdots.
$$

haskell-sicm exposes this as:

```haskell
dt :: Local Expr -> Expr -> Expr
```

For example, with polar coordinates,

```haskell
x =
    r * cos theta

xdot =
    dt state x
```

the package derives

$$
\dot x
=
\dot r\cos\theta
-
r\dot\theta\sin\theta
$$

from the local-state information.

This is a key step toward writing coordinate transformations directly and letting the library propagate derivatives automatically.

---

## 5.4 `SICM.Mechanics.Lagrange`

### Partial derivatives

For structured coordinates:

```haskell
partial q expression
```

computes the structured derivative of a scalar expression.

If `q` is an `up` structure, the gradient-like derivative is naturally returned as a `down` structure.

### Euler-Lagrange operator

For

$$
L=L(t,q,\dot q),
$$

the Euler-Lagrange expression is

$$
E(L)
=
D_t
\left(
\frac{\partial L}{\partial\dot q}
\right)
-
\frac{\partial L}{\partial q}.
$$

haskell-sicm implements this as:

```haskell
eulerLagrange
    :: Local Expr
    -> Expr
    -> Structure Expr
```

For a one-dimensional harmonic oscillator,

$$
L
=
\frac12m\dot x^2
-
\frac12kx^2,
$$

the code can be written schematically as:

```haskell
lagrangian =
    (1 / 2) * m * xdot ** 2
        - (1 / 2) * k * x ** 2

equation =
    eulerLagrange state lagrangian
```

and the result is equivalent to

$$
m\ddot x+kx.
$$

Setting the Euler-Lagrange expression to zero gives

$$
m\ddot x+kx=0.
$$

### Lagrange-d'Alembert interpretation

If only the kinetic term is supplied,

$$
L=T,
$$

then

$$
D_t
\left(
\frac{\partial T}{\partial\dot q}
\right)
-
\frac{\partial T}{\partial q}
=
Q
$$

can be interpreted as the generalized force required to produce a specified motion.

This interpretation is used in the Kepler example before gravitational potential energy has been assumed.

---

# 6. The action and stationary-action principle

`SICM.Mechanics.Action` connects paths directly to the variational formulation of mechanics.

A path is:

```haskell
type Path =
    Expr -> Structure Expr
```

and a Lagrangian is:

```haskell
type Lagrangian =
    Local Expr -> Expr
```

The action function has the conceptual form:

```haskell
action t t0 t1 lagrangian path
```

corresponding to

$$
S[q]
=
\int_{t_0}^{t_1}
L(\Gamma[q](t))\,dt.
$$

A varied path

$$
q_\epsilon(t)
=
q(t)+\epsilon\eta(t)
$$

is represented by:

```haskell
vary path variation epsilon
```

For endpoint-fixed variations,

$$
\eta(t_0)=\eta(t_1)=0,
$$

the first variation of the action gives

$$
\delta S
=
\int_{t_0}^{t_1}
\left[
\frac{\partial L}{\partial q}
-
D_t
\left(
\frac{\partial L}{\partial\dot q}
\right)
\right]
\eta\,dt.
$$

Because the variation is arbitrary, stationary action requires

$$
\frac{\partial L}{\partial q}
-
D_t
\left(
\frac{\partial L}{\partial\dot q}
\right)
=0,
$$

which is equivalent to the Euler-Lagrange equation.

The package exposes the functional derivative:

```haskell
functionalDerivative
```

so the connection

```text
path
  |
  v
action
  |
  v
variation
  |
  v
functional derivative
  |
  v
Euler-Lagrange equation
```

is itself executable.

The current symbolic `action` uses the package's small symbolic integrator, so it succeeds only when the resulting action integral is within that integrator's supported language. General numerical path optimization belongs to a future numerical layer.

---

# 7. Example: Kepler's laws to the inverse-square force

The main example currently included with the project is:

```text
examples/KeplerInverseSquare.hs
```

It reconstructs the inverse-square gravitational force from Kepler's observed laws rather than assuming the gravitational potential at the beginning.

The derivation proceeds approximately as follows.

### 1. Construct Cartesian coordinates from polar coordinates

$$
x=r\cos\theta,
\qquad
y=r\sin\theta.
$$

The total derivative operator computes `xdot` and `ydot`.

The prover then verifies directly that

$$
\dot x^2+\dot y^2
=
\dot r^2+r^2\dot\theta^2.
$$

No temporary substitutions such as `c = cos(theta)` and `s = sin(theta)` are required.

### 2. Use kinetic energy in Lagrange-d'Alembert form

$$
T
=
\frac12m
\left(
\dot r^2+r^2\dot\theta^2
\right).
$$

Calling

```haskell
eulerLagrange state kinetic
```

produces the generalized force required by the observed orbit.

The angular component is proved equivalent to

$$
Q_\theta
=
m\frac{d}{dt}
\left(
r^2\dot\theta
\right).
$$

### 3. Apply Kepler's second law

Equal areas in equal times imply

$$
r^2\dot\theta=h=\text{constant}.
$$

Therefore

$$
Q_\theta=0,
$$

so there is no tangential force. The force must lie along the Sun-planet radial line.

### 4. Apply Kepler's first law

For an ellipse with the Sun at a focus,

$$
r(\theta)
=
\frac{p}{1+e\cos\theta}.
$$

With

$$
u=\frac1r,
$$

symbolic differentiation gives

$$
u''+u=\frac1p.
$$

Combining this with the area law gives the radial acceleration

$$
a_r
=
-\frac{h^2}{pr^2}.
$$

Thus

$$
F_r
=
-\frac{mh^2}{pr^2}.
$$

The minus sign means the force points inward, and the magnitude is proportional to $1/r^2$.

### 5. Apply Kepler's third law

Kepler's third law makes

$$
\frac{h^2}{p}
$$

a common constant for planets orbiting the same central body. Calling that constant $\mu$,

$$
F_r
=
-\frac{m\mu}{r^2}.
$$

Newton's universal-gravity step identifies

$$
\mu=GM_{\text{Sun}}.
$$

The example finally integrates the inferred force to recover

$$
V(r)
=
-\frac{m\mu}{r}
$$

and feeds the resulting full gravitational Lagrangian back into `eulerLagrange`, closing the symbolic loop.

Run it with:

```powershell
cabal exec runghc -- -isrc examples/KeplerInverseSquare.hs
```

---

# 8. Example file organization

Example programs live under:

```text
examples/
```

Each example is intended to be a small standalone Haskell program with:

```haskell
module Main (main) where
```

The examples are not only demonstrations. They also serve as **integration tests of the public mathematical interface**.

A typical mechanics example follows this pattern:

```text
1. define symbols
2. construct coordinates
3. construct Local state
4. define geometric relations
5. use dt to obtain time derivatives
6. construct T, V, or L
7. call eulerLagrange / action
8. use prove to verify recognizable textbook identities
9. print the resulting physical conclusion
```

This organization keeps examples readable as derivations rather than as collections of implementation details.

---

# 9. Planned examples

The example library will grow as the mechanics layer grows.

Near-term examples include:

- one-dimensional harmonic oscillator;
- simple pendulum;
- particle on an inclined plane;
- two-dimensional projectile;
- particle in polar coordinates;
- general central-force motion;
- bead on a rotating hoop;
- coupled harmonic oscillators;
- double pendulum;
- stationary-action examples.

After the Hamiltonian layer is implemented, planned examples include:

- harmonic oscillator in Hamiltonian form;
- central-force Hamiltonian mechanics;
- Poisson brackets and conservation laws;
- a Hamiltonian treatment of the Kepler problem.

A longer-term goal is to implement many of the exercises and worked problems inspired by *Structure and Interpretation of Classical Mechanics* as executable Haskell examples.

The aim is not simply to print final answers. An example should expose the derivation in code: coordinates, paths, transformations, conserved quantities, equations of motion, and symbolic proofs should all remain visible.

---

# 10. Current project structure

The current source tree is conceptually organized as:

```text
src/
└── SICM/
    ├── Core/
    │   ├── Expr.hs
    │   └── Generic.hs
    │
    ├── Symbolic/
    │   ├── Function.hs
    │   ├── Differentiate.hs
    │   ├── Integrate.hs
    │   ├── Rewrite.hs
    │   ├── Simplify.hs
    │   ├── Polynomial.hs
    │   ├── Identity.hs
    │   └── Proof.hs
    │
    ├── Math/
    │   └── Structure.hs
    │
    └── Mechanics/
        ├── Local.hs
        ├── Lagrange.hs
        └── Action.hs

examples/
├── KeplerInverseSquare.hs
└── StationaryAction.hs

test/
└── Main.hs
```

Some internal modules, particularly identity/rewrite machinery, are implementation details rather than intended public vocabulary.

Future modules will be added only when they support a clear mathematical need. In particular, Hamiltonian mechanics, series/linearization, rational functions, assumptions, numerical quadrature, optimization, ODE integration, and external linear-algebra integration are natural future directions.

---

# 11. A small end-to-end example

A one-dimensional harmonic oscillator illustrates the intended style.

Mathematically,

$$
L
=
\frac12m\dot x^2
-
\frac12kx^2.
$$

In Haskell the essential part should remain close to that equation:

```haskell
let lagrangian =
        (1 / 2) * m * xdot ** 2
            - (1 / 2) * k * x ** 2

    residual =
        eulerLagrange state lagrangian
```

The expected result is

$$
m\ddot x+kx.
$$

The public proof interface is then used to check that the generated expression is the familiar equation:

```haskell
print $
    prove
        (equation === m * xddot + k * x)
```

The intended workflow is therefore:

```text
write the mechanics
        |
        v
generate symbolic mathematics
        |
        v
prove the recognizable result
```

rather than deriving everything manually and using Haskell only as a numerical calculator afterward.

---

# 12. Current limitations

haskell-sicm is still under active development.

Current limitations include:

- the simplifier is intentionally much smaller than a general CAS;
- the symbolic integrator handles a selected subset of integrals;
- rational-function canonicalization is still limited;
- assumptions such as `x > 0` or `x /= 0` are not yet a mature part of proof search;
- pretty mathematical output is still under development;
- Hamiltonian mechanics is not yet implemented;
- general numerical action minimization and numerical ODE solving are future work.

These limits are intentional. The project favors a small, readable core over rapidly accumulating opaque symbolic machinery.

---

# 13. Philosophy

The shortest description of haskell-sicm is:

> **Classical mechanics written as executable mathematics in Haskell.**

The project tries to preserve what is most interesting about SICM: coordinates can be programs, paths can be values, mathematical operators can act on those values, and a derivation can remain alive inside the source code.

Haskell gives this idea a particularly natural setting through:

- pure functions;
- algebraic data types;
- type classes;
- higher-order functions;
- immutable symbolic expressions;
- compositional program structure.

The long-term goal is a mechanics library that is small enough to understand, expressive enough to reproduce substantial SICM-style derivations, and readable enough that the source code itself can be used as a way to learn classical mechanics.

---

## License

Copyright (C) 2026 Liu Wei

This project is licensed under the GNU General Public License v3.0.
See the [LICENSE](LICENSE) file for details.
