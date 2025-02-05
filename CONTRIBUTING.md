# CONTRIBUTING #

This project accepts contributions from everyone willing to follow
these guidelines.

## Important Resources ##

- Join us in `#community-client-haskell` on the official TigerBeetle
  Slack
- Find our [Roadmap](https://github.com/agentultra/tigerbeetle-hs) in the Github wiki to find where we're going
- We use Github issues for issue management

## Development Environment Setup ##

We use Nix flakes to set up a consistent development environment with
Tigerbeetle and associated development tools available to build the
library and run tests.

Once you have the [Nix package manager](https://nixos.org/download/)
installed you can:

    $ nix develop

From the root of the project directory.  You can then fetch
Tigerbeetle's source which is needed to build and link against for
testing using the included `bin/fetch-deps.sh` script.

## Testing ##

TBD

## Submitting Changes ##

_Coming soon_: auto-formatters and hlint configuration, make sure code
passes these checks before submitting your PR for review.

Format the first line of your commit message by completing the
following prompt:

    When applied this change will... <your commit message here>

It should be stated in the present-imperative tense, begin with a
capital letter, and no period or punctuation at the end.  For example:

    Add ClientClass instance to Foobar

Leave a blank line after the first and the rest of the commit message
should explain the change.  It is good to note what was added,
changed, or removed and a brief explanation as to why.

If there is an associated issue for the commit, make sure to add the
Github markup to reference it on the final line of the message. Eg:

    Fixes: #12

When your PR is ready for review flag one of the maintainers in
[MAINTAINERS](MAINTAINERS.md).

If this is your first PR be sure to add yourself to the
[CONTRIBUTORS](CONTRIBUTORS.md) file!

## Code Style Guidelines ##

TBD

## Architecture of a TigerBeetle Client ##

We _submit_ a `tb_packet_t` with the _operation_ we want to perform to
the `tb_client` library.  That's how we create accounts, transfers,
and do queries.

``` mermaid
sequenceDiagram
    Haskell->>tb_client: submit an operation packet with on_completion_ctx 1
    tb_client->>Haskell: on_completion with on_completion_ctx 1, return tb_packet
```

We then get the response back in `tb_packet` again through the
`on_completion` callback, asynchronously.

## Conduct ##

This is an open source project and runs on volunteer power.  There
will be no belittling of anyone for any reason.  Use of hateful
language will not be tolerated.  Be kind, polite and courteous to
everyone and you'll find your PRs merged and responses to your issues
to also be kind.
