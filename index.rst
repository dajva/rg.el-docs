=========
Rg Manual
=========


``Rg.el`` is an Emacs search tool based on `ripgrep <https://github.com/BurntSushi/ripgrep>`_. It allows you to
interactively create searches, doing automatic searches based on the
editing context, refining and modifying search results and much
more. It is also highly configurable to be able to fit different
users needs.

If you are used to built-in Emacs ``rgrep`` command, transitioning to
``rg.el`` should be simple. ``Rg.el`` provides a lot of extra features
but the basics are similar.

The big benefit of using ``ripgrep`` instead of ``grep`` is
speed. Especially when searching large source code repositories
where ``ripgrep`` really shines. Please read `this blog post <http://blog.burntsushi.net/ripgrep/>`_ for some
speed comparisons with other tools.

.. toctree::
   :maxdepth: 2

   usage
   configuration
   contribute

.. _license:

License
-------

rg.el is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 3
of the License, or (at your option) any later version.

rg.el is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
:ref:`GNU General Public License <gpl>` for more details.

.. toctree::
   :maxdepth: 2

   gpl
