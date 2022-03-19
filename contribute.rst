==========
Contribute
==========


.. default-domain:: el

Contributions are very welcome. Development is done in the `GitHub
repository <https://github.com/dajva/rg.el>`_. If you find a bug, please report it in the `issue tracker <https://github.com/dajva/rg.el/issues>`_.

.. _pull_requests:

Pull requests
-------------

If you want to submit a patch, please submit a `GitHub pull
request <https://github.com/dajva/rg.el/pulls>`_. If you want to submit any larger code changes, please create an
issue first for discussion. Some features does not fit well into
this package and there is also good to agree on the general design
before doing any major work.

The minimum requirements for a pull request to be accepted is that
all existing tests pass and test coverage should not decrease. Often
a patch also needs additional tests, new/changed documentation etc.

Don't strive to submit a perfect pull request directly. It's often
better to submit something simple that shows the main direction of
the new code in order to discuss the best way to proceed and what
additions are needed.

.. _tests:

Tests
-----

`Cask <https://cask.readthedocs.io/>`_ is used for testing. The tests are written using the Emacs
built in ERT framework and executed with `ert runner <https://github.com/rejeep/ert-runner.el>`_. There are also
compilation tests, style check, package verification etc.

.. _tests_setup:

Setup
~~~~~

- `Install cask <https://cask.readthedocs.io/en/latest/guide/installation.html>`_

- Install all developer dependencies:

  .. code-block:: bash

      make deps

.. _tests_running:

Running
~~~~~~~

- Run the whole test suite:

  .. code-block:: bash

      make test

- Run only the unit/integration tests:

  .. code-block:: bash

      make ert-test

- Manually test the package with Emacs:

  .. code-block:: bash

      cask emacs -Q -L . --eval="(progn (require 'rg) (enable-default-bindings))"

.. _documentation:

Documentation
-------------

The documentation is written in org mode. The export target is
`restructured text <https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html>`_ suitable for the `Sphinx <http://www.sphinx-doc.org/en/master/>`_ documentation
generator. Sphinx is used to export the output from org mode to info
and HTML documentation. The resulting .rst files are used for the online
documentation on `https://readthedocs.io <https://readthedocs.io>`_.

The end user documentation is generated after committing to the
main repository. It's advisable to build
both html and info documentation locally and verify the output to
make sure the changes looks as expected.

.. _documentation_setup:

Setup
~~~~~

- `Install Sphinx <http://www.sphinx-doc.org/en/master/usage/installation.html>`_

  .. code-block:: bash

      sudo apt install python3-sphinx python3-sphinx-rtd-theme

- Install makeinfo

  .. code-block:: bash

      sudo apt install texinfo

.. _documentation_building:

Building
~~~~~~~~

- HTML documentation

  .. code-block:: bash

      make html

  Open ``docs/rst/_build/html/index.html`` in a browser.

- Info documentation

  .. code-block:: bash

      make info

  To view in emacs:

  .. code-block:: elisp

      C-u M-x info [RET]

  Then select the ``docs/rst/_build/info/rgel.info`` file.
