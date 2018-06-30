# electricdss-src
This includes a filtered copy of the [OpenDSS SVN repository](https://svn.code.sf.net/p/electricdss/code/trunk), including only the relevant source-code for the [dss_capi](https://github.com/PMeira/dss_capi/) project.
The upstream copy is on branch `opendss-official-svn`, while the `master` branch will contain specific modifications, including:

- Multi-platform modifications for building with FreePascal on Windows and Linux
- Match version 7 ("Classic" version, on folder `Source`) with version 8 ("Parallel Machine" version, on folder `Parallel_Version`)
- Potentially, 

This repository was created to more easily track upstream changes without all non-essential files for building `dss_capi`, resulting in less than 20 MB in total, including the (almost full) history of the OpenDSS public source-code. The full history, including binary files and more, is almost 2.3 GB.

Since the official SVN repository is partially corrupted from revisions 2142 to 2161, we skip those here to avoid further issues.
If you need to clone the official SVN to reproduce the `opendss-official-svn` branch, you can use [`git-svn`](https://git-scm.com/docs/git-svn) like this:

```
git svn clone -r1:2141 --ignore-paths="(.*\.(zip|xls|ocx|dss|DSS|CSV|csv|m|bat|Bat|BAT|ppt|dcu|pdf|PDF|doc|identcache|dll|ico|tlb|docx|bdsgroup|todo|res|png|PNG|html|bat|rc|(groupproj\.local)|groupproj|dproj|sh))|Training|Test|Deprecated_LazDSS|Doc|Distrib|Design|MyOpenDSS|(.*/__(history|recovery))|(.*Source/(cdpsm_import|x64|x86|Archive|TPerlRegEx|(DDLL/(Win32|Win64))|(CMD/lib)))" https://svn.code.sf.net/p/electricdss/code/trunk electricdss-src
cd electricdss-src
git svn fetch -r2162:HEAD
```

This will skip those bad revisions. If you need the full history, you can just remove the long `--ignore-paths="..."` parameter. For fetching new SVN branches, you can use `git svn rebase`.

