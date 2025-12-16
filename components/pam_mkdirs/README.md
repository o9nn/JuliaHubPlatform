# Overview

This is a small fork of linux-pam (1.1.8 to match Ubuntu's latest).
It adds a pam_mkdirs module that is similar to pam_mkhomedir, except that
it takes an extra `extra_dirs` option to specify additional directories to
create. E.g. to also create `/data/<username>` directories, you'd add
```
session    required     pam_mkdirs.so extra_dirs=/data
```
to your pam.d config.

# Setup
After building, simply copy the modules into the pam install dir
```
sudo cp modules/.libs/pam_mkdirs.so /lib/x86_64-linux-gnu/security/
sudo cp modules/.libs/mkdirs_helper /sbin
```
