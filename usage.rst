=====
Usage
=====


.. default-domain:: el

.. _installation:

Installation
------------

This version of *rg* is supported on GNU Emacs
25.1 or later on Linux systems. It
might work on older Emacsen and on other systems but such
configurations are not tested. Patches for other OS:es are welcome.

.. rubric:: MELPA

Packages are published on `MELPA Stable <https://stable.melpa.org/#/rg>`_ and `MELPA <http://melpa.org/#/rg>`_. From within Emacs,
run ``M-x package-install [RET] rg [RET]`` to install from those
sources.

Enable default key bindings:

.. code-block:: elisp

    (rg-enable-default-bindings)

The above will enable the default key map under the default prefix
key ``C-c s``.

.. rubric:: Manual

Releases can alternatively be downloaded from `GitHub <https://github.com/dajva/rg.el/releases/latest>`_ and installed
manually. Put all elisp files in main directory in your load path
and ``require`` the package in your init file.

.. code-block:: elisp

    (require 'rg)
    (rg-enable-default-bindings)

You would also need to make sure all package requirements are
met. For this version these are:

- **wgrep** 2.1.10

- **transient** 0.1.0

- **s** 1.10.0

- **emacs** 25.1

*rg* is using autoloaded symbols which means it's also possible
to defer loading if you have autoloading setup. That usually comes
out of the box with ``package-install``.

.. rubric:: Lazy loading

For lazy loading you don't want to call directly into the package
during startup. Use a setup similar to this instead:

.. code-block:: elisp

    (global-set-key (kbd "C-c s") #'rg-menu)
    (with-eval-after-load 'rg
       ;; Your settings goes here.
    )

If you don't want to use the transient menu interface, the following
is needed to achieve lazy loading:

.. code-block:: elisp

    ;; Workaround for emacs' lack of autoloaded keymaps.
    ;; This is essentially what use-package do.
    (defun rg-autoload-keymap ()
      (interactive)
      (if (not (require 'rg nil t))
          (user-error (format "Cannot load rg"))
        (let ((key-vec (this-command-keys-vector)))
          (global-set-key key-vec rg-global-map)
          (setq unread-command-events
    	    (mapcar (lambda (ev) (cons t ev))
    		    (listify-key-sequence key-vec))))))

    (global-set-key (kbd "C-c s") #'rg-autoload-keymap)
    (with-eval-after-load 'rg
       ;; Your settings goes here.
    )

.. rubric:: wgrep

This package use `wgrep <https://github.com/mhayashi1120/Emacs-wgrep>`_ for editing capabilities in the rg results
buffer. No setup is needed.

.. rubric:: Interaction with the *ripgrep* configuration file

The *ripgrep* binary allows using a `configuration file <https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md#configuration-file>`_ to set
default values for command line flags. This package requires
specific command line flags to function correctly and using a
*ripgrep* configuration may conflict with these requirements. Therefore
the configuration file is ignored by default. This can be changed
by the :opt:`rg-ignore-ripgreprc <rg-ignore-ripgreprc>` setting.

.. note:: Using the *ripgrep* configuration file may break functionality of this
   package if you are not careful.

.. _searching:

Searching
---------

Searching is done by invoking one of the different frontend
commands. This package is built around recursive search based on three
parameters; a single *directory*, *file type* filter, and a search
*pattern*. These three parameters can interactively be selected or
figured out automatically by the package, depending on which command
that is used.

The underlying *ripgrep* binary has the file type filter concept
built in. You have a high level of control over which files to
search and which to ignore. This is partly what makes it so fast,
ignoring uninteresting files.

In addition to the base parameters there are a lot of options that
control how a search is done. These are typically selected from the
:ref:`rg-menu <the_menu>` interface.

Case sensitivity
~~~~~~~~~~~~~~~~

Considering case when searching is an important feature of any
search tool. This package gives you a lot of control over how to
handle case sensitive and case insensitive search. It can be
forced to **on** or **off** and set to **smart case**. The latter is
similar to the *ripgrep* ``--smart-case`` flag but is not using the
flag directly. One thing to note about this is that the case
insensitive setting controls the behavior when starting a new
search. In the results buffer the setting is fixed to **on** or
**off** but can be toggled easily with a key binding. See
:opt:`rg-ignore-case <rg-ignore-case>` customization for the details of the configuration.

.. _basic_search:

Interactive search
~~~~~~~~~~~~~~~~~~

Two commands implements fully interactive search, where all the
base parameters are selected from the mini buffer.

.. command:: rg
   :kbd: C-c s r
   
   This command prompts for *query*, *file type* and *directory* and
   tries to suggest reasonable default values.
   The *query* string is interpreted as a regular expression. Default
   for *query* is the thing at point and for *directory* it is the current
   directory.
   If the type of the currently visited file is recognized, the
   corresponding :ref:`file type alias <file_type_aliases>` is suggested as the *file type*
   parameter.
   
   Invoking this command with the *universal argument* will trigger
   confirmation and potential modification of the :ref:`full command line <full_command_line_search>`
   that will invoke the *ripgrep* binary.

.. command:: rg-literal
   :kbd: C-c s t
   
   This command works in the same way as :cmd:`rg <rg>` but interprets the *query*
   string literally and not as a regular expression.
   
   Invoking this command with the *universal argument* will trigger
   confirmation and potential modification of the :ref:`full command line <full_command_line_search>`
   that will invoke the *ripgrep* binary.

.. _project_search:

Project search
~~~~~~~~~~~~~~

A common scenario is to search through a whole project while
visiting a file in the project. This essentially means identifying
the project root and use that as the top *directory* when invoking
the *ripgrep* binary. *rg* supports several ways of identifying a
project. Emacs' major project packages are supported including
`projectile <https://www.projectile.mx/en/latest/>`_, `find-file-in-project <https://github.com/technomancy/find-file-in-project>`_ and builtin `project.el <https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el>`_. If
none of these are used, the fallback is Emacs' ``vc-backend``.

.. command:: rg-project
   :kbd: C-c s p
   
   Search in the current project. The *directory* is selected via one
   of Emacs' project packages while *query string* and *file type*
   are prompted for. The *query string* is interpreted as a regular
   expression.

.. _do_what_i_mean:

Do what I mean
~~~~~~~~~~~~~~

The **DWIM** family of search commands tries to be smart by figure
out the search parameters from the context without
prompting. Thanks to *ripgrep's* speed, this allows for new ways of
searching by invoking a dwim command and then *refine* the
search from the results buffer.

These commands use the word (with the definition of word depending
on context) under cursor as the *query* string. The *file type*
parameter is taken from the type of the currently visited file. If
the current file type can not be identified all file types known
to *ripgrep* are used. The fallback can be customized with
:opt:`rg-default-alias-fallback`. The *directory* parameter varies
between these commands.

.. command:: rg-dwim-project-dir
   
   Do a **DWIM** search in the current :ref:`project <project_search>`.

.. command:: rg-dwim-current-dir
   
   Do a **DWIM** search in the current directory.

.. command:: rg-dwim-current-file
   
   Do a **DWIM** search in the current file. The *current file* in this
   context is actually a file *pattern* exactly matching the current
   file name in a search starting from current directory. Most of the
   time this means a single file but if there are multiple files with
   the same name in a sub directory, those will be searched as well.

.. command:: rg-dwim
   :kbd: C-c s d
   
   This command combines all the **DWIM** commands to one. The default
   search is in the :cmd:`project dir <rg-dwim-project-dir>`. With one *universal argument* :cmd:`current
   directory <rg-dwim-current-dir>` is used and with double *universal arguments* a :cmd:`file
   search <rg-dwim-current-file>` is done.

.. _file_type_aliases:

File type aliases
~~~~~~~~~~~~~~~~~

File type aliases are used in *ripgrep* to filter out the files
to search in. The *ripgrep* binary comes with a default set
of aliases that can be extended or overridden from this package by
customizing :opt:`rg-custom-type-aliases`.

An alias is a mapping between a name and a list of `glob patterns <https://en.wikipedia.org/wiki/Glob_%2528programming%2529>`_
matching the files of interest. Selecting an alias when searching
is done with completing read of the defined aliases. It is also
possible to enter a custom glob pattern if there is no suitable
alias defined for the file type.

*rg* defines some internal aliases:

.. table::

    +----------------+------------------------------------------------------------------------------------+
    | Name           | Meaning                                                                            |
    +================+====================================================================================+
    | **all**        | all defined types including :opt:`rg-custom-type-aliases <rg-custom-type-aliases>` |
    +----------------+------------------------------------------------------------------------------------+
    | **everything** | all files. No filtering on type is done.                                           |
    +----------------+------------------------------------------------------------------------------------+
    | **custom**     | used internally in this package for mapping custom glob patterns.                  |
    +----------------+------------------------------------------------------------------------------------+

.. warning:: Do not use any of the internal aliases in :opt:`rg-custom-type-aliases <rg-custom-type-aliases>`.
   That would interfere with the package internal usage.

.. _the_menu:

The menu
~~~~~~~~

The global :opt:`prefix key <rg-keymap-prefix>` may be bound to a transient
prefix command, which in normal words mean that the key binding
will popup a menu. This package is using the same `popup menu
backend <https://magit.vc/manual/transient>`_ as the `magit <https://magit.vc/manual/magit>`_ package. If you are familiar with magit this
should feels like home.

The menu is mostly interesting when you want to give specific
command line flags to the *ripgrep* binary. When you just want to do
a quick search based on the defaults the menu basically acts as a
normal keymap.

Pressing the ``rg-menu`` :opt:`prefix key <rg-keymap-prefix>` will popup the menu where command
line flags can be selected before triggering the wanted search
function. The menu can be customized via the transient API as
usual. This package contains some shortcuts to directly add a new
command to the menu when defining the command via the
:func:`rg-define-search` macro.

.. code-block:: elisp

    (rg-define-search rg-word
      :format literal
      :flags ("--word-regexp")
      :menu ("Custom" "w" "Word"))

The ``:menu`` keyword in the above invocation will trigger insertion
of a new menu item bound to key ``w`` with description **Word**. The
new menu item will be put under the **Custom** group. This group is
not available in the original menu so it will be created.

The menu can be triggered from the :ref:`results buffer <results_buffer>` with the ``m`` key.
The commands in the menu differs, depending on from where it's
triggered but the available options are the same. The menu does
not show all options by default. The visible options can be
controlled by the transient suffix levels documented `here <https://magit.vc/manual/transient/Enabling-and-Disabling-Suffixes.html#Enabling-and-Disabling-Suffixes>`_.

.. _results_buffer:

Results buffer
--------------

The results of a search is shown in the results buffer. This buffer
displays search parameters, the full command line and the output of
the *ripgrep* binary. It supports basic navigation between search
results editing of the file contents directly from the search
buffer and also modification of the current search. The results
buffer is a modified *compilation* buffer and some key bindings and
functionality is inherited from the parent and from *grep mode*.

Navigation
~~~~~~~~~~

Navigation works mostly as in grep/compilation buffers.

.. command:: compilation-next-error
   :kbd: M-n
   
   Move to next line with a match.

.. command:: compilation-previous-error
   :kbd: M-p
   
   Move to previous line with a match.

.. command:: next-error-no-select
   :kbd: n
   
   Move to next line with a match, show that file in other buffer and highlight the
   match.

.. command:: previous-error-no-select
   :kbd: p
   
   Move to previous line with a match, show that file in other buffer and highlight the
   match.

.. command:: rg-next-file
   :kbd: C-n
   
   Move to next file header if the results is grouped under a file
   header (See :opt:`rg-group-result`).

.. command:: rg-prev-file
   :kbd: C-p
   
   Move to previous file header if the results is grouped under a file
   header (See :opt:`rg-group-result`).

.. command:: compilation-next-file
   :kbd: }
   
   Move first match in previous file.

.. command:: compilation-previous-file
   :kbd: {
   
   Move last match in previous file.

.. command:: compile-goto-error
   :kbd: RET
   
   Visit match in file.

Refine search
~~~~~~~~~~~~~

From the results buffer it's easy to change the search
parameters. Some bindings toggle a flag while others allow you to
interactively change the :ref:`base
parameters <searching>`.

.. command:: rg-rerun-change-dir
   :kbd: d
   
   Interactively change search *directory*.

.. command:: rg-rerun-change-files
   :kbd: f
   
   Interactively change searched *file types*.

.. command:: rg-rerun-change-literal
   :kbd: t
   
   Interactively change *search string* interpret the string literally.

.. command:: rg-rerun-change-regexp
   :kbd: r
   
   Interactively change *search string* interpret the string as a regular
   expression.

.. tip:: :cmd:`rg-rerun-change-regexp` and :cmd:`rg-rerun-change-literal` are
   used for switching between regular expression and literal
   search. So for quick switching between search modes with the same
   search string,  just press the respective key and then ``RET``.

.. command:: rg-recompile
   :kbd: g
   
   Rerun the current search without changing any parameters.

.. command:: rg-rerun-toggle-case
   :kbd: c
   
   Toggle case sensitivity of search. The state of the flag is shown
   in the **[case]** header field.

.. command:: rg-rerun-toggle-ignore
   :kbd: i
   
   Toggle if ignore files are respected. The state of the flag is shown
   in the **[ign]** header field.

.. tip:: It is possible to create and bind your own toggle flags with the
   macro :func:`rg-define-toggle`.

.. command:: rg-menu
   :kbd: m
   
   Fire up :ref:`the menu <the_menu>` for full access to options and flags.

.. _full_command_line_search:

Full command line search
~~~~~~~~~~~~~~~~~~~~~~~~

Some search commands (See :cmd:`rg` or :cmd:`rg-literal`) allow you to
edit the final command line before invoking the search by giving a
*universal argument*. This can be used to invoke features of the
*ripgrep* binary that is not supported in this package's
interface. This could be specific flags, searching in multiple
directories etc.

.. note:: Using full command line search will disable refinement of the
   search from the result buffer.

.. _history_navigation:

History navigation
~~~~~~~~~~~~~~~~~~

Each search result is stored in the search history, which is a per
results buffer property. History can be navigated back and
forward, the forward history is cleared when a new search is done.

.. command:: rg-back-history
   :kbd: C-c <
   
   Navigate back in history.

.. command:: rg-forward-history
   :kbd: C-c >
   
   Navigate forward in history.

.. tip:: The key bindings here are slightly inconvenient so invoking this
   via :ref:`the menu <the_menu>` by pressing ``m b`` and ``m w`` is more ergonomic.

.. _edit_and_apply:

Edit and apply (wgrep)
~~~~~~~~~~~~~~~~~~~~~~

The results buffer supports inline editing via the `wgrep <https://github.com/mhayashi1120/Emacs-wgrep>`_
package. This is setup automatically when *rg* is loaded.

.. command:: wgrep-change-to-wgrep-mode
   :kbd: e
   
   Make the search results editable by enabling ``wgrep`` mode.
   When done press ``C-c C-c`` to commit your changes to the underlying
   files or ``C-c C-k`` to drop the changes.

.. _search_management:

Search management
-----------------

The result buffer is named ``*rg*`` and *rg* reuse the same result buffer for new
searches. If you want to store a search while continuing doing new searches
there are two ways of doing that.

.. command:: rg-save-search
   :kbd: s
   
   Save the search buffer by renaming it to a unique new name.
   This is available both outside and inside a result buffer. Outside
   of the result buffer it's bound to
   ``C-c s s``.
   
   If you want to keep all search buffers until manually killed you can
   use this snippet in your init file.
   
   .. code-block:: elisp
   
       (defadvice rg-run (before rg-run-before activate)
         (rg-save-search))

.. command:: rg-save-search-as-name
   :kbd: S
   
   Save the search buffer and interactively give it a specific name.
   This is available both outside and inside a result buffer. Outside
   of the result buffer it's bound to
   ``C-c s S``.

The default buffer name can be customized with :opt:`rg-buffer-name`. This
setting considers dir local variables and it's even possible to use
a function to get a really dynamic setup.

Having a lot of search buffers floating around can easily get
messy. To help keeping this under control there is a search
manager. The manager is simply a modified ``ibuffer`` that lists all
the results buffers, shows some data about the searches and make it
possible to kill of some unused etc.

.. command:: rg-list-searches
   :kbd: l
   
   Open the search manager.
   This is available both in result buffer and globally bound to
   ``C-c s l``.

.. command:: rg-kill-saved-searches
   :kbd: C-c s k
   
   Kill all saved searches except for the one that matches :opt:`rg-buffer-name`.
   This is available both in result buffer and globally bound to
   ``C-c s k``.

.. warning:: If you have a dynamic :opt:`rg-buffer-name` setup, only one buffer that
   matches your current criteria (dir locals or project for instance)
   will be kept. So be careful when killing saved searches to avoid
   losing important search results.
