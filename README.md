# Alternating Games 

## How To Run 

While having this folder (wherein the README file resides) as the current 
working directory, use one of the following commands: 

```bash
dotnet run --project src/FSharpApp/FSharpApp.fsproj <input-file>

# or

make run <input-file>
```

where `<input-file>` is a relative or absolute path to a file containing a 
railway network. (Hint: A lot of examples in `src/TestSuite/input/):

```bash
dotnet run --project src/FSharpApp/FSharpApp.fsproj src/TestSuite/input/10-lyngby-more-trains.txt 
```

F# implementation for bachelor project with the following title: 

> A game-based platform for synthesis of programs controlling railway systems.

And the following explanation: 

> A game-based platform should be developed for generation of programs that can
> control railway systems. These programs should be correct by construction.
