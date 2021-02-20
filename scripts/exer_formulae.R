#Forumale model the interaction of a dependent and independent variable
#
#
for1 <- y ~ x

#Using minus sign ignores that object
#
for2 <- y ~ x1 - x2

#Asterisk model interactons
#
for3 <- y ~ x1 *x2

#Adding  a 3-way interction in cludes both 2 and 3-way interactions, and so on.
#Interactoin is modeled by multiplying the values of the interactors together, then multiplying by a coefficient
 y ~ X1 *x2 *x3
 
 #: models interaction but not the indivudal variables. This gives only the product of x1 and x2
 
 y ~ x1:x2

#Add 0 or -1 to a regression formula to drop intercept
#
 y ~ 0 +x1 +x2
 
 #Using ^ models that level of interactions, up to 3. This models only 2-way intearctions
 
 
 Y ~ X*Z*W - X:Z:W
 Y ~ (X + Z + W)^2
 #Intercept may also be offset
 y ~ x1 + offset(rep(3, n))
 
 #Formulae handle factors by automatically creating dummies, using factor's 
 #first level as baseline. This means dummies can be created by wrapping in factor:
 
 
 y ~ factor(x)
 
 #Crucially, dormulae do not evaluate thier contents. To force evaluation of a term, use I()
 #
 y ~ x + I(x^2)
 
 #I is also usefol for rescaling:
 #
y ~ I(2 * x)

#all.vars gives vars in formula
all.vars(y ~ x1 + x2)

#Use update to modify formulae without converting to character. Note use of . 
update(y ~ x, ~. + x2)

update(y ~ x, z ~ .)