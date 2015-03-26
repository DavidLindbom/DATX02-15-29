
todo:
Adapt CodeGen to new TCModule

adapt pipeline to accommodate imports, typeclasses
add [Name],[Type] field to RenamedModule,
look in 'interface files' to locate FQN of e.g. Just
(because codegen needs to know module)
What is an interface file you ask?
Why it's just the TCModule!