<!-- Short optional summary -->

Compatible with [`jormungandr@{{JORM_TAG}}`](https://github.com/input-output-hk/jormungandr/releases/tag/{{JORM_TAG}}) and [`cardano-node@{{CARDANO_NODE_TAG}}`](https://github.com/input-output-hk/cardano-node/releases/tag/{{CARDANO_NODE_TAG}}).

<!-- A CHANGELOG, organized in three sections:

 - New Features
 - Improvements
 - Resolved Issues

-->

{{CHANGELOG}}

## Known Issues

<!-- Bugs known at the moment of the release, or discovered after and not fixed -->

## Documentation

<!-- A snapshot of the documentation at the time of releasing. -->

Cardano (cardano-node)                                                                                             | ITN (Jörmungandr)
---                                                                                                                | ---
[API Documentation](https://input-output-hk.github.io/cardano-wallet/api/{{GIT_TAG}})                              | [API Documentation](https://input-output-hk.github.io/cardano-wallet/api/{{GIT_TAG}})
[CLI Manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface/{{WIKI_COMMIT}}) | [CLI Manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface-jormungandr/{{WIKI_COMMIT}})
[Docker Manual](https://github.com/input-output-hk/cardano-wallet/wiki/Docker/{{WIKI_COMMIT}})                     | [Docker Manual](https://github.com/input-output-hk/cardano-wallet/wiki/Docker-jormungandr/{{WIKI_COMMIT}})

## Installation Instructions

<!-- Specific installation steps for this particular release. This should
basically captures whatever is currently available on the repository at
the moment of releasing. -->

### Cardano (cardano-node)

1. Install [`cardano-node@{{CARDANO_NODE_TAG}}`](https://github.com/input-output-hk/cardano-node/releases/tag/{{CARDANO_NODE_TAG}}).

2. Download the provided `cardano-wallet` for your platform, and uncompress it in a directory that is on your `$PATH`, e.g. `/usr/local/bin`. Or `%PATH%` on Windows.

4. Start `cardano-wallet --help` and see available parameters.

#### Docker

Pull from DockerHub and verify the version matches {{CABAL_VERSION}}.

```
$ docker pull inputoutput/cardano-wallet:{{CABAL_VERSION}}
$ docker run --rm inputoutput/cardano-wallet:{{CABAL_VERSION}} version
```

### ITN (jormungandr)

1. Install [`jormungandr@{{JORM_TAG}}`](https://github.com/input-output-hk/jormungandr/releases/tag/{{JORM_TAG}}).

2. Download the provided `cardano-wallet-jormungandr` for your platform, and uncompress it in a directory that is on your `$PATH`, e.g. `/usr/local/bin`. Or `%PATH%` on Windows.

3. (optional) Install the bash/zsh auto-completion script according to the [jormungandr cli manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-Command-Line-Interface/{{JORM_CLI_WIKI_COMMIT}})

4. Start `cardano-wallet --help` and see available parameters.

#### Docker

Pull from DockerHub and verify the version matches {{CABAL_VERSION}}

```
$ docker pull inputoutput/cardano-wallet:{{CABAL_VERSION}}-jormungandr
$ docker run --rm inputoutput/cardano-wallet:{{CABAL_VERSION}}-jormungandr version
```

## Signatures

<!-- Signatures of people responsible for the release -->

Name                           | Role                | Approval
---                            | ---                 | ---:
Matthias Benkort @KtorZ        | Technical Team Lead | :hourglass:
Piotr Stachyra @piotr-iohk     | QA Engineer         | :hourglass:
Tatyana Valkevych @tatyanavych | Release Manager     | :hourglass:
