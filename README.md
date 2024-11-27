## SWI-Prolog Java Record Converter 

This program processes class definitions from a .txt file and generates corresponding .h and .cpp files in a specified directory. The program reads a structured input file containing **Java Records**, creates a target directory for the output, and writes the generated header and source files. It's designed to streamline the creation of C++ class boilerplate code based on a simple input format.

---
### Steps to run the program
#### 1. Install Prolog Interpreter 
- Ensure you have a Prolog interpreter installed, such as SWI-Prolog. If not, you can download it in the following link:
``` 
  https://www.swi-prolog.org/download/devel
```
#### 2. Create an input file 
- Make sure that it has a **.txt** extension and is located in the same directory as the recordConverter.pl file. 
#### 3. Add the **Java Records** you want to turn into C++ classes 
- Here are various examples of valid and invalid record structure for the program to convert: 
#### <ins>Valid inputs
``` 
      public record Person(String name, int age, int id, float height) { } 
      public record Person(String name, int age, Address address) { public static record Address(String street, String city, String zipCode) { } } 
```
#### <ins>Invalid inputs
``` 
          public record Plane(int captainId, int planeId, int passengerAmount, boolean available) { 
              
          }

          public record Person(String name, int ager, Address address) {
            public static record Address(String street, String city, String zipCode) { }
          }     
```
*<ins>Important note</ins>: as for nested records and in general, as long as the record structure is in a single line, it should not cause any issues*

#### 4. Execute the program
- Open the command-line in the directory where 'recordConverter.pl' is  located.
- In the command-line type the following command:
```
swipl -s recordConverter.pl -g recordConverter -t halt
```
- Then, when prompted, type the name of your input file and click the enter key :)
#### 5. Check the Output

- The program creates a **<ins>Generated Classes</ins>** directory. Within this directory, you will find the .h and .cpp files generated for each record described in the input file.
- Each .h and .cpp file will have a name to match that of the original **Java Record**.
---
### Developer Contact
- *Joel Ramirez Vargas*
  - Email: <ins>joelramva07@gmail.com 