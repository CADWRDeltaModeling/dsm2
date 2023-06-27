# DSM2 Coding Guidelines
DSM2 suite is a publicly-owned open-source project, and it is open to contributions. To maintain the project efficiently, we encourage to follow project coding guidelines as below when you make contributions.

# How to contribute
A standard way to contribute to DSM2 suite repository is via pull requests (PRs) in GitHub. Feel free to generate PRs.

If you need to work closely with us and to make frequent commits directly to the repository, ask our team to join the contributor group.

# Coding Standards
For all languages we use, follow coding styles below.

- No tabs. Use spaces.
- Keep constant indentation style per file at least. We do not enforce fixed indent sizes universally, but often we use 4 spaces for indents.
- Use `LF` line-ending. See the git guideline to run how to manage it through git.
- Keep the line length limited to a certain column. We recommend the maximum in between 80 to 132 letter per line.
- Use a space between a operator and operands. Use a space after a comma. (Some exceptions can be used such as operators in function arguments or array indices to make them look more compact.)
- Use ample amount of comments to explain codes. Document functions/subroutines/classes well with documents including arguments and return variables. Use automatic documentation tools if they are available, for example, Javadocs and Python documentation.
- Remove tailing whitespaces.

## Coding Recommendations
The following coding principles are well-known programming recommendations. We encourage to follow these.

- Do not write lengthy functions. Break up your codes in functions/subroutines/etc with appropriate lengths. If you have to scroll up and down a lot to understand codes, it indicates that codes can be modulized better.
- It is ideal to make a function do only one task.
- Do not repeat or copy-and-paste codes. (Often it is referred as DRY principle - Do not repeat yourself.)
- Avoid multiple levels of loops and if statement branches (so-called deep nesting).
- Use good variable and function names that can be understood easily about what they do from the names. Codes themselves are the best documentation. Avoid short or shortened names (unless they are obvious iteration variables).
- Avoid hard-wired values. If you have to, use parameters/enumerators/constant variables with comments.
- Avoid global variables. Global variables are hard to maintain, and often they degrade performance, too.
- Test your codes as much as possible before your codes are shared. Write tests if they are not available.

## Tools to Help Code Formatting
You can use any tools or editors that you like for coding, but there are some tools that can help you to format your codes. We recommend
 VS Code because it has various choices of extensions that can help us in coding and styling and because it has great community supports. Thus, this guideline explains a few tips and tools with VS Code.

## General Formatting
### Line Ending
At the line ends of a text file, there are invisible special characters that indicate line changes. Unfortunately, depending on operating systems, these special characters can be different. (See [Newline - Wikipedia](https://en.wikipedia.org/wiki/Newline) to find out more about it.) They make no differences to our eyes and compilers, but changes in the line endings are recognized as differences and changes by version control programs such as git. Thus, it is good to keep one line ending styles, especially codes are developed over multiple operating systems. Our standard of the line ending is Unix style, `LF` only. So, please __use LF or Linux line ending style with your editor__.

To mitigate this conveniently, git can convert the line endings automatically when files are added. On Windows, set a git global option as below:

    $ git config --global core.autocrlf true

When one file uses mixture of line ending characters, the git option may not work as you wanted. If that is the case, please convert the line endings manually to make them consistent. In VS Code, you can click CRLF or LF in the right end of the bottom status bar, and choose LF, or select `Change end of line sequence` in the command palette to do so.

Please `git diff` when you add and commit your changes. It will show any unwanted changes such as line endings.

### Tailing whitespaces
When tailing whitespaces at the end of lines are left in codes, they can be recognized as changes by version control systems (VCSs) even though they are not visible in an editor. So, it is a good idea to remove tailing whitespaces. VS Code has extensions for this, and one of them is `Trailing Spaces`. It will highlight trailing whitespaces, and you can remove them with extensions.

## For Fortran
Currently, we do not have strict coding styles for Fortran codes because Fortran in DSM2 suite is a mixture of Fortran 77, Fortran 90 and later over a long history of development. We, however, recommend using Fortran 90 for future development. Here are some coding guidelines:

- Use free from of Fortran 90 and later.
- Limit the line length if you can. The length of 132 is a good choice.
- Use `intent` keywords for subroutine/function arguments.
- Avoid `goto`s.

For VS Code, we recommend `Modern Fortran` extension and `fpretty`. See the documentation of `Modern Fortran` to find out how to set up the extension and tools for it such as a language server.

## For Java
We do not have a strict coding style for Java. For the future Java development, it is recommended to follow a style suggested in [Google Java Style Guide](https://google.github.io/styleguide/javaguide.html) or [Oracle Java Code Conventions](https://www.oracle.com/technetwork/java/codeconventions-150003.pdf). If Eclipse is your Java IDE, try its auto-formatter.

# Git Standards
We use git for version-controlling, and there are a few important guidelines regarding git usage.

- Frequent commits and pushes are preferred. It prevents from losing your work, and it promotes quick sharing of work. In turn, collaborators can work more efficiently with less conflicts. Small changes also help us understand what changes are made.
- Though small commits are definitely better than a large commit with many changes in one, it is a good idea to have one logical chunk of code changes in one commit. You may want to rearrange your local commits to combine or split them via rebasing before pushing them.
- Add good commit messages. See a section below for good git commit messages.
- We use a trunk-based development. Avoid creating and maintaining long-lived branches.
  - Use 'branching-by-abstraction' to guard new features.
- Use rebasing when you pull, instead of merging. It helps us maintain a nice linearly-formed master branch. A linear branch history often is easier to follow than merged branches are.
- Use git `autocrlf` configuration to maintain one end-of-line style. Our end-of-line is LF only (which is Linux line-ending).

## Good Git Commit Messages
To understand the commit history and review commits better, it is important to write good commit messages. There are no definite rules for git commit messages, but recommendations below are commonly followed among developers. (The list is copied from [cbeams - How to Write a Git Commit Message](https://cbea.ms/git-commit/)).

- Separate subject from body with a blank line
- Limit the subject line to 50 characters
- Capitalize the subject line
- Do not end the subject line with a period
- Use the imperative mood in the subject line
- Wrap the body at 72 characters
- Use the body to explain what and why vs. how