# AFP RWH book coverage

Table of Contents
01. Chapter 1 Getting Started
01.1. Your Haskell Environment
01.2. Getting Started with ghci, the Interpreter
01.3. Basic Interaction: Using ghci as a Calculator
01.4. Command-Line Editing in ghci
01.5. Lists
01.6. Strings and Characters
01.7. First Steps with Types
01.8. A Simple Program

02. Chapter 2 Types and Functions
02.01. Why Care About Types?
02.02. Haskell’s Type System
02.03. What to Expect from the Type System
02.04. Some Common Basic Types
02.05. Function Application
02.06. Useful Composite Data Types: Lists and Tuples
02.07. Functions over Lists and Tuples
02.08. Function Types and Purity
02.09. Haskell Source Files, and Writing Simple Functions
02.10. Understanding Evaluation by Example
02.11. Polymorphism in Haskell
02.12. The Type of a Function of More Than One Argument
02.13. Why the Fuss over Purity?
02.14. Conclusion

03. Chapter 3 Defining Types, Streamlining Functions
03.01. Defining a New Data Type
03.02. Type Synonyms
03.03. Algebraic Data Types
03.04. Pattern Matching
03.05. Record Syntax
03.06. Parameterized Types
03.07. Recursive Types
03.08. Reporting Errors
03.09. Introducing Local Variables
03.10. The Offside Rule and Whitespace in an Expression
03.11. The case Expression
03.12. Common Beginner Mistakes with Patterns
03.13. Conditional Evaluation with Guards

04. Chapter 4 Functional Programming
04.01. Thinking in Haskell
04.02. A Simple Command-Line Framework
04.03. Warming Up: Portably Splitting Lines of Text
04.04. Infix Functions
04.05. Working with Lists
04.06. How to Think About Loops
04.07. Anonymous (lambda) Functions
04.08. Partial Function Application and Currying
04.09. As-patterns
04.10. Code Reuse Through Composition
04.11. Tips for Writing Readable Code
04.12. Space Leaks and Strict Evaluation

05. Chapter 5 Writing a Library: Working with JSON Data
05.01. A Whirlwind Tour of JSON
05.02. Representing JSON Data in Haskell
05.03. The Anatomy of a Haskell Module
05.04. Compiling Haskell Source
05.05. Generating a Haskell Program and Importing Modules
05.06. Printing JSON Data
05.07. Type Inference Is a Double-Edged Sword
05.08. A More General Look at Rendering
05.09. Developing Haskell Code Without Going Nuts
05.10. Pretty Printing a String
05.11. Arrays and Objects, and the Module Header
05.12. Writing a Module Header
05.13. Fleshing Out the Pretty-Printing Library
05.14. Creating a Package
05.15. Practical Pointers and Further Reading

06. Chapter 6 Using Typeclasses
06.01. The Need for Typeclasses
06.02. What Are Typeclasses?
06.03. Declaring Typeclass Instances
06.04. Important Built-in Typeclasses
06.05. Automatic Derivation
06.06. Typeclasses at Work: Making JSON Easier to Use
06.07. Living in an Open World
06.08. How to Give a Type a New Identity
06.09. JSON Typeclasses Without Overlapping Instances
06.10. The Dreaded Monomorphism Restriction
06.11. Conclusion

07. Chapter 7 I/O
07.01. Classic I/O in Haskell
07.02. Working with Files and Handles
07.03. Extended Example: Functional I/O and Temporary Files
07.04. Lazy I/O
07.05. The IO Monad
07.06. Is Haskell Really Imperative?
07.07. Side Effects with Lazy I/O
07.08. Buffering
07.09. Reading Command-Line Arguments
07.10. Environment Variables

08.06. An important Aside: Writing Lazy Functions

09. Chapter 9 I/O Case Study: A Library for Searching the Filesystem
09.01. The find Command
09.02. Starting Simple: Recursively Listing a Directory
09.03. A Naive Finding Function
09.04. Predicates: From Poverty to Riches, While Remaining Pure
09.05. Sizing a File Safely
09.06. A Domain-Specific Language for Predicates
09.07. Controlling Traversal
09.08. Density, Readability, and the Learning Process
09.09. Another Way of Looking at Traversal
09.10. Useful Coding Guidelines

10. Chapter 10 Code Case Study: Parsing a Binary Data Format
10.1. Grayscale Files
10.2. Parsing a Raw PGM File
10.3. Getting Rid of Boilerplate Code
10.4. Implicit State
10.5. Introducing Functors
10.6. Writing a Functor Instance for Parse
10.7. Using Functors for Parsing
10.8. Rewriting Our PGM Parser
10.9. Future Directions

11. Chapter 11 Testing and Quality Assurance
11.1. QuickCheck: Type-Based Testing
11.2. Testing Case Study: Specifying a Pretty Printer
11.3. Measuring Test Coverage with HPC

12.09. Life Without Arrays or Hash Tables

13. Chapter 13 Data Structures
13.01. Association Lists
13.02. Maps
13.03. Functions Are Data, Too
13.04. Extended Example: /etc/passwd
13.05. Extended Example: Numeric Types
13.06. Taking Advantage of Functions as Data
13.07. General-Purpose Sequences

14. Chapter 14 Monads
14.01. Revisiting Earlier Code Examples
14.03. Looking for Shared Patterns
14.04. The Monad Typeclass
14.05. And Now, a Jargon Moment
14.06. Using a New Monad: Show Your Work!
14.07. Mixing Pure and Monadic Code
14.08. Putting a Few Misconceptions to Rest
14.09. Building the Logger Monad
14.10. The Maybe Monad
14.11. The List Monad
14.12. Desugaring of do Blocks
14.13. The State Monad
14.14. Monads and Functors
14.15. The Monad Laws and Good Coding Style

15. Chapter 15 Programming with Monads
15.01. Golfing Practice: Association Lists
15.02. Generalized Lifting
15.03. Looking for Alternatives
15.04. Adventures in Hiding the Plumbing
15.05. Separating Interface from Implementation
15.06. The Reader Monad
15.07. A Return to Automated Deriving
15.08. Hiding the IO Monad

18. Chapter 18 Monad Transformers
18.01. Motivation: Boilerplate Avoidance
18.02. A Simple Monad Transformer Example
18.03. Common Patterns in Monads and Monad Transformers
18.04. Stacking Multiple Monad Transformers
18.05. Moving Down the Stack
18.06. Understanding Monad Transformers by Building One
18.07. Transformer Stacking Order Is Important
18.08. Putting Monads and Monad Transformers into Perspective

19. Chapter 19 Error Handling
19.01. Error Handling with Data Types
19.03. Error Handling in Monads

## Below is "cursory reading" - was part of AFP before 2012 or so

24. Chapter 24 Concurrent and Multicore Programming
24.01. Defining Concurrency and Parallelism
24.02. Concurrent Programming with Threads
24.03. Simple Communication Between Threads
24.04. The Main Thread and Waiting for Other Threads
24.05. Communicating over Channels
24.06. Useful Things to Know About
24.07. Shared-State Concurrency Is Still Hard
24.08. Using Multiple Cores with GHC
24.09. Parallel Programming in Haskell
24.10. Parallel Strategies and MapReduce

28. Chapter 28 Software Transactional Memory
28.01. The Basics
28.02. Some Simple Examples
28.03. STM and Safety
28.04. Retrying a Transaction
28.05. Choosing Between Alternatives
28.06. I/O and STM
28.07. Communication Between Threads
28.08. A Concurrent Web Link Checker
28.09. Practical Aspects of STM
