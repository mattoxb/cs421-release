# CS421 Haskell Programming Assignments

This repository contains the programming assignments (MPs) for CS421. Throughout the course, you'll complete Haskell assignments that explore functional programming concepts and implement programming language features.

## Getting Started

### 1. Create Your Repository

You should have gotten instructions on how to create a repository on the illinos coursework github account.
See https://cs421-sp26-web.pages.dev/Docs/Your-GitHub-Repository to accomplish this.

### 2. Add this Repository as Upstream

cd into your new repository and do this:

```
git remote add upstream https://github.com/mattoxb/cs421-release.git
```

Verify your remotes:

```bash
git remote -v
# Should show:
# origin    https://github.com/YOUR-USERNAME/cs421-release.git (fetch)
# origin    https://github.com/YOUR-USERNAME/cs421-release.git (push)
# upstream  https://github.com/mattoxb/cs421-release.git (fetch)
# upstream  https://github.com/mattoxb/cs421-release.git (push)
```

### 4. Set Up Your Development Environment

You have three options for setting up your Haskell development environment:

**Option 1: VSCode + Docker (Recommended)**

This is the easiest way to get started without installing Haskell tools locally:

1. Install [Docker Desktop](https://www.docker.com/products/docker-desktop/)
2. Install [VS Code](https://code.visualstudio.com/)
3. Install the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
4. Open this repository in VS Code
5. When prompted, click "Reopen in Container" (or press `Ctrl+Shift+P` and select "Dev Containers: Reopen in Container")

See [STUDENT_SETUP.md](./STUDENT_SETUP.md) for detailed instructions and troubleshooting.

**Option 2: Docker Only**

If you prefer working in a terminal without VS Code:

```bash
docker pull mattox/cs421-haskell-dev:latest
docker run -it -v "$(pwd):/home/student/workspace" mattox/cs421-haskell-dev:latest
```

**Option 3: Nix Package Manager**

If you use Nix, the included `flake.nix` provides all necessary dependencies:

```bash
nix develop
```

## Receiving New Assignments

When new assignments (mp2, mp3, etc.) are released, pull them from the upstream remote:

```bash
# Fetch updates from the course repository
git fetch upstream

# Merge updates into your local branch
git merge upstream/main  # or upstream/master, depending on branch name
```

If you've modified starter code, you may encounter merge conflicts. Resolve them carefully to preserve both your work and the new assignment files.

## Working on Assignments

### Repository Structure

Each assignment is in its own directory:

- `mp1a-recursion/` - Part 1: Recursion and higher-order functions
- `mp1b-adts/` - Part 2: Algebraic data types
- More assignments will be added throughout the semester

### Running Your Code

Start the Haskell REPL (inside the Docker container or after setting up Haskell locally):

```bash
cd mp1a-recursion  # or whichever assignment directory
stack ghci
```

### Testing Your Code

Run the test suite for an assignment:

```bash
cd mp1a-recursion  # or whichever assignment directory
stack test
```

See individual assignment README files for detailed instructions and problem descriptions.

## Submitting Your Work

### Committing to Your Fork

Commit your work regularly to your forked repository:

```bash
git add .
git commit -m "Complete mytake and mydrop functions"
git push origin main  # Push to YOUR fork, not upstream
```

Note: You cannot (and should not) push to the `upstream` remote. That's the course repository maintained by the instructor.

### Submission Instructions

Git is not used for submission.  It may be super helpful during office hours though.  Actual submission
happens via prairielearn.

## Getting Help

- Check the individual assignment READMEs for problem-specific guidance
- See [STUDENT_SETUP.md](./STUDENT_SETUP.md) for environment setup help
- Ask on the class Discord or attend office hours
- If you're stuck on a problem, you can define it as `undefined` to keep your code compiling:
  ```haskell
  myFunction :: [a] -> [a]
  myFunction = undefined
  ```

## Important Notes

- **Code that doesn't compile receives a zero.** Always ensure your code compiles, even if you need to use `undefined` for incomplete functions.
- **Push to `origin` (your fork), not `upstream`** - The upstream remote is read-only for students
- **Commit regularly** - Don't lose your work!
- **Keep your fork private**. This is required by course policy to prevent academic integrity issues

## Workflow Summary

```bash
# One-time setup
git clone https://github.com/YOUR-USERNAME/cs421-release.git
cd cs421-haskell
git remote add upstream https://github.com/mattoxb/cs421-release.git

# When new assignments are released
git fetch upstream
git merge upstream/main

# While working on assignments
cd mp1a-recursion
stack ghci          # Interactive development
stack test          # Run tests

# Committing your work
git add .
git commit -m "Descriptive message"
git push origin main
```

Happy coding!
