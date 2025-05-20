

obj="21APR2025"


vec_example=c("My","name","is","Anthony","GonzalViz")


names(vec_example)=c("A","B","C","D","E")

vec_example["B"]

list_of_objects=ls()
mode(list)

is.vector(list)

print(list)
print(list_of_objects)
rm(list=ls())

vec_ex=c("A"=1,"B"=2)


invisible(vec_ex[["A"]])

user=Sys.info()[["user"]]
user
