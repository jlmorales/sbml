# SBML

#sbml.py is an interpreter for a programming language that is a combination of python and sml

#here are some examples of programs that you can run

#EXAMPLE 1:

fun factorial(n) = {
    if(n <1){
        output = 1;
    }else{
        output = n * factorial(n - 1);
    }
} output;

{
    print(factorial(3));
}

#EXAMPLE 2

fun gcd(a,b) = {
    t = b;
    b = a mod b;
    if(b == 0){
        output = t;
    } else {
        output = gcd(t,b);
    }
} output;

{

    print(gcd(32,18));
    
}



#You can run the program by opening the command line and running python sbml.py file.txt
#where file is the file.txt is the file you are trying to run
