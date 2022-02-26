# Chapter 3. Names, Scopes, Bindings

---


## 3-1
 
### Q

Indicate the binding time (when the language is designed, when the program is linked, when the program begins execution, etc.) for each of the following decisions in your favorite programming language and implementation. 
Explain any answers you think are open to interpretation.

- The number of built-in functions (math, type queries, etc.)
- The variable declaration that corresponds to a particular variable reference (use)
- The maximum length allowed for a constant (literal) character string
- The referencing environment for a subroutine that is passed as a parameter
- The address of a particular library routine
- The total amount of space occupied by program code and data



### A

My favorite programming language is Rust.


> The number of built-in functions (math, type queries, etc.)

Language design time.

> The variable declaration that corresponds to a particular variable reference (use)

Program writing time.

> The maximum length allowed for a constant (literal) character string

Rust takes constant(literal) character string as a String slice(&str), and that is reference to program data's string bynary.
And it has max limit: `isize::MAX as usize` -> this is binded at language implementation time.

> The referencing environment for a subroutine that is passed as a parameter

Runtime.

> The address of a particular library routine

Link time.

> The total amount of space occupied by program code and data

Load time.


---


## 3-2
 
### Q

In Fortran 77, local variables were typically allocated statically.
In Algol and its descendants (e.g., Ada and C), they are typically allocated in the stack.
In Lisp they are typically allocated at least partially in the heap.
What accounts for these differences? Give an example of a program in Ada or C that would not work correctly if local variables were allocated statically.
Give an example of a program in Scheme or Common Lisp that would not work correctly if local variables were allocated on the stack.


### A






---


## 3-3
 
### Q

Give two examples in which it might make sense to delay the binding of an implementation decision, even though sufficient information exists to bind it early.

### A

1. Polymorphism
  - Language implementation can delay method binding to runtime.
  - Polymorphism makes the object can decide what method would be runned in runtime, so this provide a lot of flexibility.

2. Dynamic linking
  - Usually linking(static link) is done in compile time, dynamic linking is done in runtime.
  - Dynamic linking is slower but space-efficient: if same library is used by many process(program file), the program files can save its memory using dynamic linking.

---


## 3-4
 
### Q

Give three concrete examples drawn from programming languages with which you are familiar in which a variable is live but not in scope.


### A

Javascript case.

Actually I want to do this exercise with Rust, but I can't get to the answer easily.
So I think with Javascript.

1. module: names in module is live, but not in scope if current scope is out of the module
2. shadowing: names which is shadowed is live, but inner scope same name make it out of scope.
3. object which reference count is 0: before garbage collection, the object which RC = 0 is live but not in scope.


---

## 3-5
 
### Q

Consider the following pseudocode:
```
1. procedure main()
2.     a : integer := 1
3.     b : integer := 2

4.     procedure middle()
5.         b : integer := a
6.         procedure inner()
7.             print a, b
8.         a : integer := 3

9.         – – body of middle
10.        inner()
11.        print a, b

12.    – – body of main
13.    middle()
14.    print a, b
```

Suppose this was code for a language with the declaration-order rules of C (but with nested subroutines)—that is, names must be declared before use, and the scope of a name extends from its declaration through the end of the block.
At each print statement, indicate which declarations of a and b are in the referencing environment. What does the program print (or will the compiler identify static semantic errors)?
Repeat the exercise for the declaration-order rules of C# (names must be declared before use, but the scope of a name is the entire block in which it is declared) and of Modula-3 (names can be declared in any order, and their scope is the entire block in which they are declared).



### A

In C:
```
1 1   // from inner(), 7th line
3 1   // from middle(), 11th line
1 2   // from main(), 14th line
```
In C#
```
5th line -> semantic error, use before declaration 
// 8th line a varialbe has scope of entire in middle()
```

In Modula-3
```
3 3   // from inner(), 7th line
3 3   // from middle(), 11th line
1 2   // from main(), 14th line
```

---


## 3-6
 
### Q

Consider the following pseudocode, assuming nested subroutines and static scope:

```
1. procedure main()
2.     g : integer

3.     procedure B(a : integer)
4.         x : integer

5.         procedure A(n : integer)
6.             g := n

7.         procedure R(m : integer)
8.             write_integer(x)
9.             x /:= 2       // –– integer division
10.            if x > 1
11.                R(m + 1)
12.            else
13.                A(m)

14.        // –– body of B
15.        x := a × a
16.        R(1)

17.    // –– body of main
18.    B(3)
19.    write_integer(g)
```

##### a. What does this program print?
##### b. Show the frames on the stack when `A` has just been called. For each frame, show the static and dynamic links.
##### c.  Explain how `A` finds `g`.

### A

##### a.answer
```
9   // 8th line
4   // 8th line
2   // 8th line
3   // 19th line
```

##### b.answer

Stack Frame's dynamic link is the reference to caller's frame and static link is the reference to lexically outer and closest scope's most recent frame.


1. Dynamic link of stack frame.
> Bookkeeping information typically includes the subroutine’s return
address, a reference to the stack frame of the caller (also called the dynamic link), ... -- book 120P

2. Static link of stack frame.
> The simplest way in which to find the frames of surrounding scopes is to maintain a static link in each frame that points to the “parent” frame: the frame of the most recent invocation of the lexically surrounding subroutine. -- book 129~130P

|stack|dynamic-link|static-link|
|-------------|:-----------:|:-----------:|
|stack 5: A(3)|R(3): stack 4|B(3): stack 1|
|stack 4: R(3)|R(2): stack 3|B(3): stack 1|
|stack 3: R(2)|R(1): stack 2|B(3): stack 1|
|stack 2: R(1)|B(3): stack 1|B(3): stack 1|
|stack 1: B(3)|main: stack 0|main: stack 0|
|stack 0: main|-------------|-------------|


##### c.answer

When `A` is called, first find `g` from `A`'s scope, but it cannot find.
So, `A` find `g` from outer scope, so it starts searching from outer closest lexical scope.
Follow to `A`'s static link, and it goes to `B: stack 1`.
Then it finds `g` in `B: stack 1`'s scope, but it isn't in there..
So go to `B: stack 1`'s static link and go to `main: stack 0`.
Then it finds `g` in `main: stack 0`'s scope, then it finds `g`.


---


## 3-7
 
### Q

As part of the development team at MumbleTech.com, Janet has written a list manipulation library for C that contains, among other things, the code in Figure 3.16.


### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---


## 3-
 
### Q




### A




---

# Explorations

