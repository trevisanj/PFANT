# DOCS directory

This directory contains the source files to generate the [PFANT documentation website](https://trevisanj.github.io/PFANT).

🚫 **Note** (20230509) dropped building the PDF manual to keep things simpler; accessing documentation from the website 
is much better anyway.

# Table of Contents

  1. [System Setup](#S1)
  2. [Rebuild](#S2)
  3. [Upload](#S3)

# <a name=S1></a>System Setup

## 1.1 Requirements

To generate HTML:

```
pip install sphinx recommonmark sphinx_rtd_theme
```

## 1.2 Clone website repository

Create directory ```PFANT-docs``` parallel to ```PFANT``` and clone branch ```gh-pages``` to directory ```PFANT-docs/html```:
```
mkdir PFANT-docs
cd PFANT-docs
git clone -b gh-pages ssh://git@github.com/trevisanj/PFANT html
```

# <a name=S2></a>2 Rebuild

## 2.1 Update version

There is no need to update the documentation version, as it will be made automatically using the date of compilation.

## 2.2 Compilation

### 2.2.1 HTML output

From ```PFANT/docs``` directory, type the following command line:

```
make html
```

# <a name=S3></a>3 Upload

```
cd ../../PFANT-docs
git push
```