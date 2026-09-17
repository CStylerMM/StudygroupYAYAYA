#Funktion der afrunder til hele tal
myRounder<-function(myNymber){
  kurt=0
  kurt=as.integer(myNymber)
  return(kurt)
}

MyNewNumber=myRounder(tal)

#Jeg forstår ikke hvad der skete:/

#Pair Programning opgave
## Exercise 3.1:

a=as.integer(c(2,5,7))
b=c(2,15,27)
c=c("2",15,27)
d=c(T,F,TRUE)
print('The following should return:integer,double,character, logical')
typeof(a)
typeof(b)
typeof(c)
typeof(d)

#a, satte vi den som as.integer på, for at få den fikset:)
#b, fjernede vi anførselstegn for at fjerne character som fællesnævner, og få den til double
#c, var korret i forvejen
#d, fjernede anførselstegn for at få false/true statements til at gælde

## Exercise 3.2 (names):
a=c(5,7,10,11,13)
print('The following should return [1] "Tal" "Tal" "Billed" "Billed" "Billed"')
names(a)<-c("tal","tal","billed","billed","billed")



## Exercise 3.3 (create matrix):
m=matrix(a,nrow=5,ncol = 5)
print(m)
print("The above should return the matrix below")
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    5    5    5    5    5
#[2,]    7    7    7    7    7
#[3,]   10   10   10   10   10
#[4,]   11   11   11   11   11
#[5,]   13   13   13   13   13
#

# forhåndsdefinerer antal kolonner og rækker

## Exercise 3.4 (lists):
ml=list(1:3,3,1.1,1:5)
print(ml)
print("The following should return: ")
# [1] 1 2 3
# 3
# 1.1

# [[1]]
# [1] 1 2 3 4 5

print(ml[1])
length(ml[1])
print(ml[3][2])
print(ml[])

#Fuck den:)

## Exercise 3.5 (named subset list):
nl=list("person"=c("navn","age"),"interests"="games")
list.tree(nl)
print("The following should return: ")
#nl = list 2 (1336 bytes)
#.  person = list 2
#. .  navn = character 1= kurt 
#. .  age = double 1= 12
#.  interests = list 1
#. .  games = character 2= FN CS 


## Exercise 3.6:
set.seed(25)
v1=runif(5,11,25)
set.seed(20)
v2=runif(3,11,25)
rl=list("person"=c("navn","age"),"runs"=list("DHL"=v1,"Erm"=v2))
print("The following should return: ")
list.tree(rl)

#rl = list 3 (1200 bytes)
#.  person = character 2= navn age 
#.  runs = list 2
#. .  DHL = double 5= 16.826 20.727 13.083 ...
#. .  Erm = double 3= 23.285 21.759 14.905 ...
#.  stats = double 1= 18.361

## Exercise 3.7:
set.seed(23)
# use runif
df=data.frame(
  age=runif(1,30,90),
  weight=runif(1,50,100)
)

print("The print statement should produce the following")
print(df)
#age weight
#1  62  72.54
#2  54 107.63
#3  57 108.58
#4  64  99.63
#5  66 109.78


## Exercise 3.8:
#  modify url below to get raw estonia-passenger-list.csv

url="https://raw.githubusercontent.com/cphstud/RIntroData/refs/heads/master/estonia-passenger-list.csv"
dfestonia=read.csv(url)
dfestonia

## Exercise 4.1:
#save row 5 of estonia into a dataframe
row5=dfestonia[5,]


print(row5)
#PassengerId Country       Firstname Lastname Sex Age Category Survived
#5           5  Sweden BRITTA ELISABET AHLSTROM   F  55        P        0

#save last row of estonia into a dataframe
rowlast=dfestonia[nrow(dfestonia),]
print(rowlast)
#command above should return
#PassengerId Country Firstname Lastname Sex Age Category Survived
#989         989  Sweden      CARL   OVBERG   M  42        P        1


## Exercise 4.2:
## Exercise 4.2.1:
#create a logical vector of length 5, alternating between T and F, starting with T
fv=rep(c(T,F),length.out=5)
fv

# use it to filter in the age-weight dataframe from excercise 3.7
df[fv,]
# it should print
#age weight
#1  62  72.54
#3  57 108.58
#5  66 109.78

#Filter out every second row of estonia using a logical vector
fv=rep(c(), length.out=(989/2))
fvE=dfestonia[fv,]
print(fvE[1:3,])
# it should print: 
#PassengerId Country       Firstname Lastname Sex Age Category Survived
#1           1  Sweden     ARVID KALLE    AADLI   M  62        P        0
#3           3 Estonia            AIRI  AAVASTE   F  21        C        0
#5           5  Sweden BRITTA ELISABET AHLSTROM   F  55        P        0


## Exercise 4.3:
# save the mean-age of estonia-passengers in a variable
# print(pasmean) should return 45
pasmean=dfestonia
print(pasmean)
