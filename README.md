# IsqGet
Retrieves UNF ISQ data as JSON from the [UNF Departmental Data Summary](https://bannerssb.unf.edu/nfpo-ssb/wkshisq.p_isq_dept_pub). Requires .NET Core 3.1+.

## Installing

First clone the project and `cd` into the project folder
```
git clone https://github.com/jonathan-lemos/IsqGet
cd IsqGet
```

### Linux
From the project folder,
```
make
sudo make install
```

### OSX
lol

### Windows
lol

## Usage
In all cases, the program will output all data to `stdout` as JSON. Errors will be outputted to `stderr`.

To see a full list of options
```
IsqGet --help
```
To get all ISQ data
```
IsqGet
```
To return only the computing department
```
IsqGet --department Computing
```
To return multiple departments
```
IsqGet --department Computing --department Math
```
To only return a specific professor
```
IsqGet --professor Arafa
```
To only return results from 2020
```
IsqGet --term 2020
```
To only return results from Summer 2020 to Fall 2020
```
IsqGet --term Summer2020-Fall2020
```
To only return specific courses
```
IsqGet --course COP3503 --course 'Calculus II'
```
