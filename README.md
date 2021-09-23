
<!-- PROJECT SHIELDS -->

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MS-PL License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/lisp-stat/dfio">
    <img src="https://lisp-stat.dev/images/stats-image.svg" alt="Logo" width="80" height="80">
  </a>

  <h3 align="center">Data Frame I/O</h3>

  <p align="center">
  A system for file and network I/O for data frames
	<br />
    <a href="https://lisp-stat.dev/docs/tasks/data-frame/"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/lisp-stat/dfio/issues">Report Bug</a>
    ·
    <a href="https://github.com/lisp-stat/dfio/issues">Request Feature</a>
    ·
    <a href="https://lisp-stat.github.io/dfio/">Reference Manual</a>
  </p>
</p>



<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li>
      <a href="#about-the-project">About the Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
	<li><a href="#resources">Resources</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About the Project

  A data frame isn't much use if you have to type all the data in by
  hand.  This system provides I/O functions to read data sets from
  delimited (CSV, TSV, etc.) files.  File may be either on local disk
  or accessible from network locations.



### Built With

* [anaphora](https://github.com/tokenrove/anaphora)
* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* [cl-csv](https://github.com/AccelerationNet/cl-csv)
* [let-plus](https://github.com/sharplispers/let-plus)


<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running follow these steps:

### Prerequisites

An ANSI Common Lisp implementation. Developed and tested with
[SBCL](https://www.sbcl.org/) and
[CCL](https://github.com/Clozure/ccl).

#### Getting the source

To make the system accessible to [ASDF](https://common-lisp.net/project/asdf/) (a build facility, similar to `make` in the C world), clone the repository in a directory ASDF knows about.  By default the `common-lisp` directory in your home directory is known. Create this if it doesn't already exist and then:

1. Clone the repository
```sh
cd ~/common-lisp && \
git clone https://github.com/Lisp-Stat/data-frame.git && \
git clone https://github.com/Lisp-Stat/dfio.git
```
2. Reset the ASDF source-registry to find the new system (from the REPL)
   ```lisp
   (asdf:clear-source-registry)
   ```
3. Load the system
   ```lisp
   (asdf:load-system :dfio)
   ```

If you have installed the slime ASDF extensions, you can invoke this
with a comma (',') from the slime REPL.

#### Getting dependencies

To get the third party systems that Lisp-Stat depends on, you can use a dependency manager, such as [Quicklisp](https://www.quicklisp.org/beta/) or [CLPM](https://www.clpm.dev/) Once installed, get the dependencies with either of:

```lisp
(clpm-client:sync :sources "clpi") ;sources may vary
```

```lisp
(ql:quickload :lisp-stat)
```

You need do this only once. After obtaining the dependencies, you can
load the system with `ASDF` as described above without first syncing
sources.

<!-- USAGE EXAMPLES -->
## Usage

Create a data frame from a file named `sg-weather.csv` on the local disk:

```lisp
(defparameter *df*
	(read-csv #P"LS:DATASETS;sg-weather.csv"))

```

For more examples, refer to the
[Documentation](https://lisp-stat.dev/docs/tasks/data-frame).


<!-- ROADMAP -->
## Roadmap

See the [open issues](https://github.com/lisp-stat/dfio/issues) for a list of proposed features (and known issues).

## Resources

This system is part of the [Lisp-Stat](https://lisp-stat.dev/) project; that should be your first stop for information. Also see the <!-- [resources](https://lisp-stat.dev/resources) and -->
[community](https://lisp-stat.dev/community) page for more
information.

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**. Please see [CONTRIBUTING](CONTRIBUTING.md) for details on the code of conduct, and the process for submitting pull requests.

<!-- LICENSE -->
## License

Distributed under the MS-PL License. See [LICENSE](LICENSE) for more information.



<!-- CONTACT -->
## Contact

Project Link: [https://github.com/lisp-stat/dfio](https://github.com/lisp-stat/dfio)



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/lisp-stat/dfio.svg?style=for-the-badge
[contributors-url]: https://github.com/lisp-stat/dfio/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/lisp-stat/dfio.svg?style=for-the-badge
[forks-url]: https://github.com/lisp-stat/dfio/network/members
[stars-shield]: https://img.shields.io/github/stars/lisp-stat/dfio.svg?style=for-the-badge
[stars-url]: https://github.com/lisp-stat/dfio/stargazers
[issues-shield]: https://img.shields.io/github/issues/lisp-stat/dfio.svg?style=for-the-badge
[issues-url]: https://github.com/lisp-stat/dfio/issues
[license-shield]: https://img.shields.io/github/license/lisp-stat/dfio.svg?style=for-the-badge
[license-url]: https://github.com/lisp-stat/dfio/blob/master/LICENSE
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/company/symbolics/
