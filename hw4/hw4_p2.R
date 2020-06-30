library(bnlearn)
dag1 = model2network("[A][B][C|A][D|A:B][E|B][F|C:A:E][G|D:E][H|F:G]")
x11()
plot(dag1)

dsep(dag1, 'C', 'G')
dsep(dag1, 'C', 'E')
dsep(dag1, 'C', 'E','G')
dsep(dag1, 'A', 'G','D')


