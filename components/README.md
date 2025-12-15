# JuliaHub Platform Components

This directory contains all the components of the JuliaHub platform, integrated as a monorepo.

## Overview

This monorepo includes 171 repositories from JuliaComputing organization, all integrated without their individual `.git` directories to create a unified JuliaHub platform.

## Components

The components were cloned from the repositories listed in `repos.tsv` at the root of this project. Each component maintains its original directory structure and files, but has been integrated into this monorepo for easier management and development.

## Integration

All components have been:
1. Cloned from their original GitHub repositories
2. Had their `.git` directories removed
3. Integrated into this monorepo structure

This allows for unified version control, easier cross-component development, and streamlined deployment of the entire JuliaHub platform.

## Cloning Process

The components are cloned in batches using the `clone_batch.jl` script in the root directory, which:
- Reads repository URLs from `repos.tsv`
- Clones repositories in batches of 20
- Removes the `.git` directory from each cloned repository
- Reports on success/failure of each clone operation

To clone a specific batch:
```bash
julia clone_batch.jl <batch_number> [batch_size]
```

For example, to clone batch 1 (repos 1-20):
```bash
julia clone_batch.jl 1 20
```

## Component List

For the complete list of components and their original repositories, see `repos.tsv` in the root directory.
