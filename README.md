`deployR-hs` : Talking to deployR from Haskell
----------------------------------------------

This project models (parts of) the deployR REST API in Haskell
using [servant](https://github.com/haskell-servant/servant).

DeployR is a software written by [Revolution-Analytics](http://revolutionanalytics.com/)
(which became part of Microsoft), and aims to solve deployment problems for
software written in R into both web APIs and asynchronous jobs.

We are not interested in the job part of deployR (for now), so these
parts of the API are not in scope (yet).

## Resources

The `deployR` API is described in
https://deployr.revolutionanalytics.com/documents/dev/api-doc/guide/single.html
and in
https://microsoft.github.io/deployr-api-docs/ .
Information about servant can be found in
http://haskell-servant.readthedocs.io/

