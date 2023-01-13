# Chapter 6. Control Flow

---

## TOC

1. [6-1](#6-1)
2. [6-2](#6-2)
3. [6-3](#6-3)
4. [6-4](#6-4)
5. [6-5](#6-5)
6. [6-6](#6-6)
7. [6-7](#6-7)
8. [6-8](#6-8)
9. [6-9](#6-9)
10. [6-10](#6-10)
11. [6-11](#6-11)
12. [6-12](#6-12)
13. [6-13](#6-13)
14. [6-14](#6-14)
15. [6-15](#6-15)
16. [6-16](#6-16)
17. [6-17](#6-17)
18. [6-18](#6-18)
19. [6-19](#6-19)
20. [6-20](#6-20)
21. [6-21](#6-21)
22. [6-22](#6-22)
23. [6-23](#6-23)
24. [6-24](#6-24)
25. [6-25](#6-25)
26. [6-26](#6-26)
27. [6-27](#6-27)
28. [6-28](#6-28)
29. [6-29](#6-29)
30. [6-30](#6-30)
31. [6-31](#6-31)
32. [6-32](#6-32)
33. [6-33](#6-33)
34. [6-34](#6-34)
35. [6-35](#6-35)
36. [6-36](#6-36)

## [6-1](#TOC)

### Q

We noted in Section 6.1.1 that most binary arithmetic operators are left-associative in most programming languages. In Section 6.1.4, however, we also noted that most compilers are free to evaluate the operands of a binary operator in either order. Are these statements contradictory? Why or why not?

### A

solution ref).

No, they are not contradictory. When there are consecutive identical operators within an expression, associativity determines which subexpressions are arguments of which operators. It does not determine the order in which those subexpressions are evaluated. For example, left associativity for subtraction determines that `f(a) - g(b) - h(c)` groups as `(f(a) - g(b)) - h(c)` (rather than `f(a) - (g(b) - h(c))`, but it does not determine whether `f` or `g` is called first.

---

## [6-2](#TOC)

### Q

As noted in Figure 6.1, Fortran and Pascal give unary and binary minus the same level of precedence. Is this likely to lead to nonintuitive evaluations of certain expressions? Why or why not?

### A

Check `-A - B` case:
In this situation, left to right evaluation applied and the full expression would be `(-A) - B`.

Check `A - -B` case:
In this situation, left to right evaluation takes `(Operand) -> A`, `(Binary Operator) -> -`, then takes next operand. So `-B` should be operand, especially a expression, so `-` in `-B` should be a unary operator. So the full expression would be `A - (-B)`.

It seems same level of precedence of unary and binary minus is not so nonintuitive.

---

## [6-3](#TOC)

### Q

In Example 6.9 we described a common error in Pascal programs caused by the fact that `and` and `or` have precedence comparable to that of the arithmetic operators. Show how a similar problem can arise in the stream-based I/O of `C++` (described in Section C-8.7.3). (Hint: Consider the precedence of `<<` and `>>`, and the operators that appear below them in the C column of Figure 6.1.)

### A

solution ref).

In Case of below Code:

```cpp
cout << a & b << endl;
```

The precedence of `<<` is higher than `&`, so expression would be

```cpp
(cout << a) & (b << endl);
```

then `(b << endl)` would throw an error. So this is the problem of operator precedence.

---

## [6-4](#TOC)

### Q

Translate the following expression into postfix and prefix notation:

```
[−b + sqrt(b × b − 4 × a × c)] / (2 × a)
```

Do you need a special symbol for unary negation?

### A

postfix: `b~ bb* ac* 4* - sqrt + 2a* /`

prefix: `/ + ~b sqrt - *bb *4 *ac *2a`

Yes, we need a special symbol for unary negation. In this case, we can use `~` for unary negation.

---

## [6-5](#TOC)

### Q

In Lisp, most of the arithmetic operators are defined to take two or more arguments, rather than strictly two. Thus `(* 2 3 4 5)` evaluates to `120`, and `(- 16 9 4)` evaluates to `3`. Show that parentheses are necessary to disambiguate arithmetic expressions in Lisp (in other words, give an example of an expression whose meaning is unclear when parentheses are removed).
In Section 6.1.1 we claimed that issues of precedence and associativity do not arise with prefix or postfix notation. Reword this claim to make explicit the hidden assumption.

### A

Lisp)
Let's check below case.

```lisp
(- 50 9 (+ 4 5 6))
```

After remove parentheses, it would be

```lisp
- 50 9 + 4 5 6
```

then it can be interpreted as

`(- 50 9 (+ 4 5) 6)` or `(- 50 9 (+ 4 5 6))` as well.
So parentheses are necessary to disambiguate arithmetic expressions in Lisp.

Prefix/Postfix Notation)

If the number of operator's operand is fixed, prefix / postfix notation makes no ambiguity. But if the number of operator's operand is variable, prefix / postfix notation makes ambiguity. For example, `+ 1 2 3` can be interpreted as `(+ 1 2) 3` or `(+ 1 2 3)`.

---

## [6-6](#TOC)

### Q

### A

---

## [6-7](#TOC)

### Q

### A

---

## [6-8](#TOC)

### Q

### A

---

## [6-9](#TOC)

### Q

### A

---

## [6-10](#TOC)

### Q

### A

---

## [6-11](#TOC)

### Q

### A

---

## [6-12](#TOC)

### Q

### A

---

## [6-13](#TOC)

### Q

### A

---

## [6-14](#TOC)

### Q

### A

---

## [6-15](#TOC)

### Q

### A

---

## [6-16](#TOC)

### Q

### A

---

## [6-17](#TOC)

### Q

### A

---

## [6-18](#TOC)

### Q

### A

---

## [6-19](#TOC)

### Q

### A

---

## [6-20](#TOC)

### Q

### A

---

## [6-21](#TOC)

### Q

### A

---

## [6-22](#TOC)

### Q

### A

---

## [6-23](#TOC)

### Q

### A

---

## [6-24](#TOC)

### Q

### A

---

## [6-25](#TOC)

### Q

### A

---

## [6-26](#TOC)

### Q

### A

---

## [6-27](#TOC)

### Q

### A

---

## [6-28](#TOC)

### Q

### A

---

## [6-29](#TOC)

### Q

### A

---

## [6-30](#TOC)

### Q

### A

---

## [6-31](#TOC)

### Q

### A

---

## [6-32](#TOC)

### Q

### A

---

## [6-33](#TOC)

### Q

### A

---

## [6-34](#TOC)

### Q

### A

---

## [6-35](#TOC)

### Q

### A

---

## [6-36](#TOC)

### Q

### A

---
