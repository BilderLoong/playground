```mermaid
graph LR;

  A:::red-->B & C

  style B stroke:#f69
  classDef red fill:#f69
  classDef blue fill:#f90


  subgraph one[name]

  classDef red fill:#f69
  classDef blue fill:#f90
  class Z,C blue

  Z --> B1
  A1 --> B2
  A1 --> B2
  C1 --> C2 --> C3;
  end

  subgraph two
  D1 --> D2
  D2 --> D1
  D2 -->C1
  end

  E1 --- E2
  E1 --- E2
```


```mermaid
graph LR

  aa--->b1
  a1 --text---- a2
  b1 --text--- b2

  q1 --"(text)"--- q2
  A["a (text)"]

  f1 ---- f2
  f1 --- f3

  e1 -.- e2
  e3 -.-> e4


```

```mermaid
graph TD; %% The statement define the type and direction of the chart.
id; %% A node.
A[asd];
A[123]; %% the last text found for the node tha will be used.
A; %% You can omit text definitions for later use.
B[hello]
```

``` mermaid
graph RL; %% BT, TD/TB, RL 
FOO(A node with round edge)
FOO-->BAR
R5[/trapezoid\]
C{ rhombus shape} 
D[[ subroutine shape]]
E[(cylindrical shape)]
F((circle shape))
G> asymmetric shape ]
R1{{hexagon shape}}
R2[/parallelogram/]
R3[\parallelogram alt\]
```

```mermaid
graph LR$$;

A1-->B1
A2---B2
A3--text in the middle---B3
A4-->|text in the middle|B4
A5--text in the middle-->B5
```
