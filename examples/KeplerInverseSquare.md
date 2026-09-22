# From Kepler's Laws to Newton's Inverse-Square Force in Haskell

This tutorial is an executable-mechanics example for our `haskell-sicm` package. The goal is deliberately the **inverse** of the usual textbook calculation.

Usually we are given Newton's force law and solve for the orbit:

```math
F(r)\longrightarrow \text{equations of motion}\longrightarrow r(t).
```

Here we start from Kepler's observed laws and ask what force a planet must experience:

```math
\text{Kepler orbit}
\longrightarrow \text{Lagrange equations}
\longrightarrow F(r).
```

The final result will be

```math
\boxed{\mathbf F=-\frac{m\mu}{r^3}\mathbf r},
\qquad
\boxed{|F|=\frac{m\mu}{r^2}},
```

where $\mu$ is the common orbital constant for planets around the same Sun. Newton's broader universal-gravity interpretation identifies

```math
\mu=GM_\odot.
```

A logical point matters here: we **must not begin by writing**
$V=-GMm/r$, because that would assume the inverse-square law we are trying
to discover. Instead we begin with the kinetic Lagrangian $L_0=T$ and use
the Lagrange-d'Alembert equation with an unknown generalized force $Q$.
Only after the force has been inferred do we integrate it to recover $V$
and the full gravitational Lagrangian.

---

## 1. Polar coordinates from Cartesian coordinates

Put the Sun at the origin. A planet at polar coordinates $(r,\theta)$ has

```math
x=r\cos\theta,\qquad
y=r\sin\theta.
```

Both $r$ and $\theta$ vary with time, so the chain rule gives

```math
\dot x
=
\frac{\partial x}{\partial r}\dot r
+
\frac{\partial x}{\partial\theta}\dot\theta
=
\dot r\cos\theta-r\dot\theta\sin\theta,
```

and

```math
\dot y
=
\dot r\sin\theta+r\dot\theta\cos\theta.
```

The Haskell code performs those derivatives rather than hard-coding them:

```haskell
x = r * cos theta
y = r * sin theta

xdot =
    totalDerivative
        [(r, rdot), (theta, thetadot)]
        x

ydot =
    totalDerivative
        [(r, rdot), (theta, thetadot)]
        y
```

The helper is simply the multivariable chain rule:

```haskell
totalDerivative rates expression =
    simplify
        (sum
            [ diff variable expression * rate
            | (variable, rate) <- rates
            ])
```

Now

```math
v^2=\dot x^2+\dot y^2.
```

Expansion gives cancellation of the cross terms:

```math
v^2
=
(\cos^2\theta+\sin^2\theta)
(\dot r^2+r^2\dot\theta^2).
```

The example actually asks our `Polynomial` module to verify this algebraic
factorization after temporarily naming
$c=\cos\theta$ and $s=\sin\theta$. Then the standard identity

```math
\cos^2\theta+\sin^2\theta=1
```

gives

```math
\boxed{v^2=\dot r^2+r^2\dot\theta^2}.
```

Therefore

```math
\boxed{
T=
\frac12m
\left(
\dot r^2+r^2\dot\theta^2
\right).
}
```

In our SICM-style structure,

```haskell
q =
    up
        [ scalar r
        , scalar theta
        ]

v =
    up
        [ scalar rdot
        , scalar thetadot
        ]

state = local t q v
```

represents

```math
(t,q,v)=
\left(
t,
\begin{pmatrix}r\\\theta\end{pmatrix},
\begin{pmatrix}\dot r\\\dot\theta\end{pmatrix}
\right).
```

The current `partial` machinery then computes the structured derivatives

```haskell
partial q kinetic
partial v kinetic
```

corresponding to

```math
\frac{\partial T}{\partial q}
=
\left(
mr\dot\theta^2,\;0
\right)
```

and

```math
\frac{\partial T}{\partial v}
=
\left(
m\dot r,\;mr^2\dot\theta
\right).
```

---

## 2. Use Lagrange without assuming gravity

For generalized coordinates $q_i$, the forced Euler-Lagrange, or
Lagrange-d'Alembert, equation is

```math
\boxed{
\frac{d}{dt}
\frac{\partial L}{\partial\dot q_i}
-
\frac{\partial L}{\partial q_i}
=
Q_i.
}
```

Because we have not yet discovered the force, take only

```math
L_0=T.
```

For the radial coordinate $r$,

```math
\frac{\partial T}{\partial\dot r}
=
m\dot r,
```

so

```math
\frac{d}{dt}
\frac{\partial T}{\partial\dot r}
=
m\ddot r.
```

Also

```math
\frac{\partial T}{\partial r}
=
mr\dot\theta^2.
```

Therefore

```math
\boxed{
Q_r=
m(\ddot r-r\dot\theta^2).
}
```

For $\theta$,

```math
\frac{\partial T}{\partial\dot\theta}
=
mr^2\dot\theta,
\qquad
\frac{\partial T}{\partial\theta}=0,
```

so

```math
\boxed{
Q_\theta=
\frac{d}{dt}(mr^2\dot\theta).
}
```

The example evaluates both of these symbolically with `diff` and
`totalDerivative`.

---

## 3. Kepler's second law tells us the direction of the force

Kepler's area law says the Sun-planet radius sweeps equal areas in equal
times.

A small swept sector has area

```math
dA=\frac12r^2d\theta,
```

so

```math
\frac{dA}{dt}
=
\frac12r^2\dot\theta.
```

Equal areas in equal times means this is constant. Define

```math
\boxed{h=r^2\dot\theta=\text{constant}}.
```

Therefore

```math
\frac{dh}{dt}=0.
```

But the angular Lagrange equation gave

```math
Q_\theta
=
m\frac{d}{dt}(r^2\dot\theta),
```

hence

```math
\boxed{Q_\theta=0}.
```

In polar coordinates $Q_\theta=rF_\theta$. Therefore, away from $r=0$,

```math
\boxed{F_\theta=0}.
```

There is no tangential force. The force must lie along the line joining the
planet and Sun.

At this stage we know the force is **radial**, but we do not yet know whether
it points inward or outward or how its magnitude depends on $r$.

---

## 4. Kepler's first law supplies the elliptical geometry

With the Sun at a focus, the ellipse can be written

```math
\boxed{
r(\theta)=
\frac{p}{1+e\cos\theta}},
```

where

```math
p=a(1-e^2).
```

Introduce the reciprocal radius

```math
u(\theta)=\frac1r.
```

Then

```math
u(\theta)
=
\frac{1+e\cos\theta}{p}
=
\frac1p+\frac{e}{p}\cos\theta.
```

Our symbolic differentiator obtains

```math
u'=-\frac{e}{p}\sin\theta
```

and

```math
u''=-\frac{e}{p}\cos\theta.
```

Consequently,

```math
\boxed{
u''+u=\frac1p.
}
```

The executable example computes this using

```haskell
u =
    1 / p + (e / p) * cos theta

uPrime =
    diff theta u

uSecond =
    diff theta uPrime

ellipseEquation =
    simplify (uSecond + u)
```

This tiny equation contains the essential geometry of the Kepler ellipse.

---

## 5. Combine the ellipse with the area law

From Kepler II,

```math
h=r^2\dot\theta.
```

Since $u=1/r$,

```math
r=\frac1u,
\qquad
r^2=\frac1{u^2},
```

so

```math
\boxed{\dot\theta=hu^2}.
```

Next,

```math
\dot r
=
\frac{dr}{d\theta}\dot\theta.
```

Because

```math
r=u^{-1},
```

we have

```math
\frac{dr}{d\theta}
=
-\frac{u'}{u^2}.
```

Thus

```math
\dot r
=
-\frac{u'}{u^2}(hu^2)
=
\boxed{-hu'}.
```

Differentiate once more:

```math
\ddot r
=
-hu''\dot\theta,
```

because $h$ is constant. Using $\dot\theta=hu^2$,

```math
\boxed{
\ddot r=-h^2u^2u''.
}
```

The polar radial acceleration is not merely $\ddot r$. It is

```math
a_r=\ddot r-r\dot\theta^2.
```

The second term is

```math
r\dot\theta^2
=
\frac1u(h^2u^4)
=
h^2u^3.
```

Therefore

```math
\begin{aligned}
a_r
&=
-h^2u^2u''-h^2u^3\\
&=
-h^2u^2(u''+u).
\end{aligned}
```

Now use the ellipse result

```math
u''+u=\frac1p:
```

```math
a_r
=
-\frac{h^2}{p}u^2.
```

Finally $u=1/r$, so

```math
\boxed{
a_r
=
-\frac{h^2}{p\,r^2}.
}
```

Therefore

```math
\boxed{
F_r
=
m a_r
=
-\frac{mh^2}{p\,r^2}.
}
```

This gives both requested properties.

The positive $r$-direction points outward from the Sun, so the negative
sign means

```math
\boxed{\text{the force points toward the Sun}.}
```

Its magnitude is

```math
\boxed{
|F_r|
=
\frac{mh^2/p}{r^2}
\propto\frac1{r^2}.
}
```

The inverse-square law has appeared without assuming Newtonian gravity.

---

## 6. Kepler's third law fixes the common strength

Kepler II also relates $h$ to the period. Since

```math
\frac{dA}{dt}=\frac h2,
```

one complete orbit of area $A$ and period $T$ gives

```math
A=\frac h2T,
```

therefore

```math
h=\frac{2A}{T}.
```

For an ellipse,

```math
A=\pi ab,
```

so

```math
h=\frac{2\pi ab}{T}.
```

Using

```math
b^2=a^2(1-e^2)
```

and

```math
p=a(1-e^2),
```

we obtain

```math
\frac{h^2}{p}
=
\frac{4\pi^2a^3}{T^2}.
```

Kepler's third law says

```math
\frac{T^2}{a^3}=K
```

for all planets around the same Sun. Hence

```math
\frac{h^2}{p}
=
\frac{4\pi^2}{K}
```

is the same constant for every planet. Call it

```math
\boxed{\mu}.
```

Thus

```math
\boxed{
F_r=-\frac{m\mu}{r^2}.
}
```

In vector form,

```math
\boxed{
\mathbf F
=
-\frac{m\mu}{r^3}\mathbf r.
}
```

The vector formula makes the direction especially clear: the force is a
negative multiple of the Sun-to-planet position vector.

Kepler's laws themselves establish the central inverse-square form and the
common solar parameter $\mu$. Identifying

```math
\mu=GM_\odot
```

and extending the same law to arbitrary gravitating masses is the additional
Newtonian universality step.

---

## 7. Recover the potential with our symbolic integrator

Now that the force has been discovered, we may finally introduce a
conservative potential.

```math
F_r=-\frac{dV}{dr}.
```

Since

```math
F_r=-\frac{m\mu}{r^2},
```

we have

```math
\frac{dV}{dr}
=
\frac{m\mu}{r^2}.
```

The Haskell code asks our integrator to recover $V$:

```haskell
dVdr =
    m * mu * r ** (-2)

potential =
    integrate r dVdr
```

giving, up to an irrelevant additive constant,

```math
\boxed{
V(r)=-\frac{m\mu}{r}.
}
```

Therefore the gravitational Lagrangian is

```math
\boxed{
L
=
\frac12m
(\dot r^2+r^2\dot\theta^2)
+
\frac{m\mu}{r}.
}
```

Notice the plus sign in $L$: since $L=T-V$ and
$V=-m\mu/r$, the potential contributes $+m\mu/r$.

---

## 8. Close the loop with the ordinary Euler-Lagrange equation

Now the force has been absorbed into $V$, so the ordinary unforced equation

```math
\frac{d}{dt}
\frac{\partial L}{\partial\dot q_i}
-
\frac{\partial L}{\partial q_i}
=0
```

is sufficient.

For $r$, the Haskell machinery produces the equation equivalent to

```math
m\ddot r
-
mr\dot\theta^2
+
\frac{m\mu}{r^2}
=
0.
```

Therefore

```math
m(\ddot r-r\dot\theta^2)
=
-\frac{m\mu}{r^2},
```

which is exactly the inferred radial force.

For $\theta$,

```math
\frac{d}{dt}(mr^2\dot\theta)=0,
```

which reproduces Kepler's area law / angular-momentum conservation.

We have therefore closed the logical loop:

```math
\boxed{
\begin{array}{c}
\text{Kepler I: ellipse}\\
\text{Kepler II: area law}\\
\text{Kepler III: period law}
\end{array}}
```

```math
\Downarrow
```

```math
\boxed{
\mathbf F=-\frac{m\mu}{r^3}\mathbf r
}
```

```math
\Downarrow
```

```math
\boxed{
V=-\frac{m\mu}{r}
}
```

```math
\Downarrow
```

```math
\boxed{
L=
\frac12m(\dot r^2+r^2\dot\theta^2)
+\frac{m\mu}{r}
}
```

and Euler-Lagrange returns the same dynamics.

---

## Running the executable example

Place `KeplerInverseSquare.hs` under the project's `examples/` directory and
run from the project root:

```powershell
cabal exec runghc -- -isrc examples/KeplerInverseSquare.hs
```

The example intentionally uses only the package pieces we have already
implemented:

- `Expr`
- `Differentiate`
- `Simplify`
- `Polynomial`
- `Integrate`
- `Structure`
- `Local`
- `Lagrange.partial`

The full reusable Euler-Lagrange operator is not yet part of
`SICM.Mechanics.Lagrange`, so the example contains a small local
`lagrangeLeft` helper. That helper is exactly the operation we are about to
promote into the package proper.
