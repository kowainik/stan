# Contributing to Plu‑Stan

Thank you for your interest in contributing! We welcome all kinds of help: issues, bug fixes, enhancements, documentation, etc.

## How to Contribute

### 1. Raising & Managing Issues

- Before you file a new issue, please search existing issues to avoid duplicates.
- If your idea is a new feature or enhancement, label it clearly (e.g. `enhancement`, `proposal`).
- Provide enough context: what you intend, problem, possible approaches.
- We encourage linking to specs/designs or motivating examples.
- If the issue doesn't target Plinth, we encourage you to file it to Stan project instead, and we can rebase it later from there

### 2. Commits & Branching Standards

We follow **Conventional Commits** (https://www.conventionalcommits.org) to maintain a clean history:

- Format:  
  ```
  <type>(<scope>?): <short summary>

  <optional body>

  <optional footer(s)>
  ```
- Example:  
  ```
  feat(anti-pattern): avoid unsing XXXXX

  The body can explain motivation, backward compatibility, etc.

  BREAKING CHANGE: <description of breaking change>
  ```
- Common types:
  - `feat` — new feature  
  - `fix` — bug fix  
  - `docs` — documentation only  
  - `style` — formatting, white-space, etc (no logic changes)  
  - `refactor` — code change that neither fixes a bug nor adds a feature  
  - `test` — adding or updating tests  
  - `chore` — maintenance, tooling, etc  
- Use a meaningful **scope**, usually a module or component (e.g. `parser`, `anti-pattern`, `analyzer`).
- For breaking changes, use the `BREAKING CHANGE:` footer and bump version accordingly in `CHANGELOG.md`.
- Maintain atomic commits: prefer small, logical units of change.

### 3. Pull Requests (PRs)

When opening a PR, please:

1. **Base branch**  
   - Prefer working off `main` (or an active development branch, if specified).  
   - If your change is large or experimental, open it as a **draft PR** and mark it clearly.

2. **Title & Description**  
   - Title should follow Conventional Commits style (e.g. `feat(cli): add subcommand "analyze"`).  
   - In the PR body, include:
     - A short summary of what it changes.
     - The issue(s) solved (if any). Use `Closes #123` or `Fixes #456` where applicable.
     - If no existing issue covers it, reference its purpose or motivation (link design/issue if created).
     - Any relevant discussion or context (design decisions, trade-offs, etc).
     - Instructions for testing or verification (if applicable).

3. **Reviewer(s)**  
   - Every PR **must** be reviewed by at least one reviewer (ideally someone familiar with that area).
   - For substantial changes, request review from domain experts or core maintainers.

4. **Tests / CI**  
   - All PRs **must include tests** for new features or bug fixes. 
   - If the tests involves new AntiPatterns, add the corresponding example in the `./target` project and a test in the  `./test/Test/Stan/Analysis/PlutusTx.hs` 
   - If you are only doing formatting, doc changes, or minor tweaks, tests may not be required—but still check whether core behavior is affected.
   - If there are no tests for your change path (e.g. new area), flag the PR as `draft` and discuss adding a test harness.

5. **Commit Hygiene**  
   - Squash or clean up your commits before merging; maintain a clean history.
   - Rebase in case `main` moved (avoid unnecessary merge commits).
   - Ensure commit messages follow the conventional commit format.

### 4. Review & Merging

- After you open the PR, reviewers may comment or ask for changes.
- Please address review comments in timely fashion, pushing new commits or rebasing as needed.
- Once approved and CI passes, a maintainer may merge (or ask you to squash & merge).
- After merge, you may delete your branch (if appropriate).

### 5. Code Style, Linting & Formatting

- plu-stan uses `stylish-haskell` for Haskell code formatting.We recommend setting up your editor to auto-format on save.

### 6. Documentation, Examples & Tutorials

- Contributions to docs, examples, are highly welcome.
- Please keep examples up to date with changes.
- If adding a new feature, consider adding sample usage in README or other docs.

### 7. Staying in Sync & Communication

- Subscribe to issue threads or PRs you care about.
- If you plan a large change, consider opening a design issue first so the team can provide feedback early.
- Let maintainers know if you intend to work on something to avoid duplicating efforts.

---
