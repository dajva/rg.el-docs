=============
Configuration
=============


.. default-domain:: el

.. _customization:

Customization
-------------

Customization is done via the Emacs customization system. The group
``rg`` is the main group of the package.

.. code-block:: elisp

    M-x customize-group [RET] rg [RET]

.. option:: rg-executable
   :default: (executable-find "rg")
   
   The *ripgrep* executable to use. Could be an absolute path or just the
   base name if the executable is in the path. The default is using
   ``executable-find`` to locate the command. If you want to use this
   package with tramp it might be better to set it to just "rg" in
   order to let the OS find the binary where it's invoked.
   From Emacs 27.1, the tramp use case is by default handled
   automatically. See :opt:`rg-executable-per-connection` for details.

.. option:: rg-executable-per-connection
   :default: t
   
   This setting only has effect in Emacs 27.1 or later.
   Handle the :opt:`rg-executable` automatically for different hosts if used
   with tramp. ``executable-find`` for "rg" binary will be invoked on
   remote hosts to determine the path to ripgrep. The result is stored
   per connection.

.. option:: rg-custom-type-aliases
   :default: nil
   
   An association list that maps file type aliases to a space
   delimited string with file globs. These are combined with the
   *ripgrep* builtin file aliases.
   
   Example:
   
   .. code-block:: elisp
   
       (setq rg-custom-type-aliases
         '(("foo" .    "*.foo *.bar")
           ("baz" .    "*.baz *.qux")))
   
   You may also add lambdas to ``rg-custom-type-aliases`` to add aliases
   dynamically based on mode, directory, project, etc.
   
   .. code-block:: elisp
   
       (add-to-list
        'rg-custom-type-aliases
        (lambda ()
          (when (in-frontend-app)
            (cons "ui" "*.js *.hbs *.json"))))

.. option:: rg-prioritized-type-aliases
   :default: nil
   
   A list of aliases that are prioritized among ripgrep's builtin
   aliases when selecting the alias based on the buffer file name. This
   list contains only the alias names and the order between the items
   does not matter.
   
   Example:
   
   .. code-block:: elisp
   
       (setq rg-custom-type-aliases
         '("cpp" "puppet"))

.. option:: rg-default-alias-fallback
   :default: "everything"
   
   This setting controls the default alias used when no alias can be
   recognized for the current buffer. ``all`` or ``everything`` are
   reasonable values for this variable.

.. option:: rg-command-line-flags
   :default: nil
   
   A list of command line flags that will be appended to the
   *ripgrep* command line. Must either be a list of flags or a function
   that returns a list of flags.

.. option:: rg-group-result
   :default: t
   
   
   Controls the layout of the results buffer. If non ``nil``, each file name
   is displayed once and matches are grouped under that filename instead of
   repeating the filename on each match. This is essentially the layout of
   the ``--no-heading`` *ripgrep* command line flag.

.. option:: rg-show-columns
   :default: nil
   
   
   Controls if column numbers are used in the search result.

.. option:: rg-ignore-case
   :default: case-fold-search
   
   Setting that controls if case sensitive search is made or not. It
   can essentially be **on**, **off** or **smart**. The **smart** setting will
   trigger an analyze of the search string and if it's all lower case,
   the search will be case *insensitive*, otherwise it will be case
   *sensitive*. The following values are valid:
   
   - **case-fold-search** - A non nil value of ``case-fold-search`` will trigger smart case behavior.
   
   - **smart** - Smart case behavior.
   
   - **force** - Always ignore case.
   
   - **nil** - Always consider case.

.. option:: rg-hide-command
   :default: t
   
   Hide most of command line by default. This is enabled by default and can
   be set to ``nil`` to show full command line.
   This can be toggled in the results buffer by clicking on the command line.

.. option:: rg-keymap-prefix
   :default: "C-c s"
   
   This variable sets the default prefix used for the global key bindings.
   Note that ``rg-enable-default-bindings`` needs to be invoked for the
   bindings to be enabled.

.. option:: rg-use-transient-menu
   :default: t
   
   Controls whether ``rg-menu`` will be used by default or not. It's also
   possible to enable the menu explicitly with
   
   .. code-block:: elisp
   
       (rg-enable-menu)

.. option:: rg-show-header
   :default: t
   
   Controls if the search info header is shown in the result buffer. This
   is enabled by default but can be disabled by setting this variable to
   ``nil``.

.. option:: rg-buffer-name
   :default: "rg"
   
   Controls the name of the results buffer. It may be *string* or *function*.
   This name will be surrounded by  ``*`` to yield the final buffer name
   so if this setting is ``foo`` the buffer name will be ``*foo*``.
   One useful case of using it is to have separate result buffers per project.
   One can set this variable in \`dir-locals\` file or set it to function.
   
   Example, this function will set results buffer name based on \`project-current\`:
   
   .. code-block:: elisp
   
       (defun my-rg-buffer-name ()
         (let ((p (project-current)))
           (if p
       	(format "rg %s" (abbreviate-file-name (cdr p)))
             "rg")))

.. option:: rg-ignore-ripgreprc
   :default: t
   
   Controls if the `ripgreprc <https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md#configuration-file>`_ file should be ignored or not. If ``nil``,
   the config file will be used, otherwise it will be ignored. The
   default is to ignore this file in order to avoid that conflicting
   settings have impact on this package's behavior. Setting this to ``nil``
   may affect core functionality of this package. Especially changing
   colors can affect parsing of the output and result in a broken
   results buffer.

.. _position-numbers-alignment:

Position numbers alignment
~~~~~~~~~~~~~~~~~~~~~~~~~~

When operating *rg* in grouped output mode (:opt:`rg-group-result` is non
nil), it's possible to control how the line and column numbers are
displayed in the result buffer.

Example settings:

.. code-block:: elisp

    (setq rg-align-position-numbers t)
    (setq rg-align-line-number-field-length 3)
    (setq rg-align-column-number-field-length 3)
    (setq rg-align-line-column-separator "#")
    (setq rg-align-position-content-separator "|")

Will yield the following format:

::

    File: matched_file.foo
      1#  2|match1
    888# 10|match2

.. option:: rg-align-position-numbers
   :default: t
   
   Setting this to ``t`` will align line and column numbers in columns padded
   with white space.

.. option:: rg-align-line-number-field-length
   :default: 4
   
   
   Defines the length of the line number field.

.. option:: rg-align-column-number-field-length
   :default: 3
   
   
   Defines the length of the column number field.

.. option:: rg-align-line-column-separator
   :default: " "
   
   
   Separator string used between line and column numbers. ``nil`` means
   use default separator from *ripgrep*.

.. option:: rg-align-position-content-separator
   :default: " "
   
   Separator string used between the position numbers and matched content. ``nil`` means
   use default separator from *ripgrep*.

Faces
-----

All faces are in the subgroup ``rg-face`` of the main group ``rg``.

.. code-block:: elisp

    M-x customize-group [RET] rg-face [RET]

Results buffer
~~~~~~~~~~~~~~

.. option:: rg-match-face
   :default: match
   
   Face used to highlight matches in result.

.. option:: rg-error-face
   :default: compilation-error
   
   Face used to highlight errors when invoking *ripgrep*.

.. option:: rg-context-face
   :default: shadow
   
   Face used to highlight context lines in *ripgrep* output when
   ``--context-lines`` flag is used.

.. option:: rg-info-face
   :default: compilation-info
   
   Face used to highlight general info in results buffer. For instance
   the number of matches found.

.. option:: rg-warning-face
   :default: compilation-warning
   
   Face used to highlight warnings in the *ripgrep* output.

.. option:: rg-filename-face
   :default: rg-info-face
   
   Face used to highlight filenames in the output.

.. option:: rg-file-tag-face
   :default: rg-info-face
   
   Face used for the ``File:`` tag in grouped results output.

.. option:: rg-line-number-face
   :default: compilation-line-number
   
   Face used on line numbers.

.. option:: rg-column-number-face
   :default: compilation-column-number
   
   Face used on column numbers.

.. option:: rg-match-position-face
   :default: default
   
   Face added to file positions. This is the start of a matching line
   and depending on configuration may be, file name, column number and
   line number.

.. _header_line_config:

Header line
~~~~~~~~~~~

.. option:: rg-toggle-on-face
   :default: rg-file-tag-face
   
   Face used for flags that are toggled ``on``.

.. option:: rg-toggle-off-face
   :default: rg-error-face
   
   Face used for flags that are toggled ``off``.

.. option:: rg-literal-face
   :default: rg-filename-face
   
   Face used the on the ``literal`` marker in the header line.

.. option:: rg-regexp-face
   :default: compilation-line-number
   
   Face used the on the ``regexp`` marker in the header line.

.. _configuration_functions:

Configuration functions
-----------------------

.. function:: (rg-enable-default-bindings &optional prefix)
   
   Enable the default keyboard bindings for the package with prefix
   key. If :opt:`rg-use-transient-menu` is on this will enable the menu
   instead of activating the global bindings. If ``prefix`` is not
   provided :opt:`rg-keymap-prefix` will be used.

.. function:: (rg-enable-menu &optional prefix)
   
   Enable the :ref:`rg-menu <the_menu>` with prefix key. This bypass
   :opt:`rg-use-transient-menu` setting. If ``prefix`` is not provided
   :opt:`rg-keymap-prefix` will be used.

.. function:: (rg-use-old-defaults )
   
   This function is provided to keep backwards compatibility with
   versions older than 2.0.0. In this version default settings as well
   as key bindings changed and to bring back the old defaults call this
   function in your init file.

.. _hooks:

Hooks
-----

.. option:: rg-finish-functions
   :default: nil
   
   Functions to call when a ripgrep search is finished.
   
   Each function is called with two arguments: the compilation buffer,
   and a string describing how the process finished.

.. option:: rg-filter-hook
   :default: nil
   
   Hook run after new content has been inserted in in the rg buffer.
   This hook is called every time the rg buffer has been updated with
   new content and filtered internally by the package.

.. option:: rg-mode-hook
   :default: (wgrep-rg-setup)
   
   Hook run after entering rg mode.

.. _configuration_macros:

Configuration macros
--------------------

.. function:: (rg-define-toggle flag &optional key default)
   
   This is a macro that can be used to define custom *ripgrep* flag
   toggling functions in the result buffer. The macro takes the flag
   (and potential value) as an argument and optionally binds the toggle
   function to a key. If ``default`` is non nil the flag is used by default.
   
   The function defined by this macro will be named as the flag name
   stripped with leading dashes and prefixed with ``rg-custom-toggle-flag-``.
   
   .. code-block:: elisp
   
       (rg-define-toggle "-uu" "I" t)
   
   Creates a function named ``rg-custom-toggle-flag-uu`` that is on by
   default and bound to ``I`` in *rg* result
   buffer.
   
   .. code-block:: elisp
   
       (rg-define-toggle "--context 3" (kbd "C-c c"))
   
   Creates a function named ``rg-custom-toggle-flag-context`` that is off by
   default and bound to ``C-c c`` in *rg* result
   buffer.

.. function:: (rg-define-search name &rest args)
   
   This macro can be used to define custom search functions in a
   declarative style. Default implementations for common behavior is
   available and custom forms can also be used.
   
   It optionally starts with a string that is used as the docstring for
   the defined function.  The rest of the arguments contain key value pairs
   according to the specification below.  All keys are optional with
   specified default if left out.
   
   - **:query** - Method for retrieving the search string.  Allowed values are
     ``point`` which means extract thing at point and ``ask`` which means
     prompt the user for a string.  Any form that evaluates to a string
     is allowed. Default is ``ask``.
   
   - **:format** - Specifies if ``:query`` is interpreted literally
     (``literal``) or as a regexp (``regexp``). If it is a form, eg.
     ``(not current-prefix-arg)``, and is non-nil the ``:query`` is interpreted
     literally, otherwise as a regexp. Default is ``regexp``.
   
   - **:files** - Form that evaluates to a file alias or custom file
     glob. ``current`` means extract alias from current buffer file name,
     ``ask`` will prompt the user. Default is ``ask``.
   
   - **:dir** - Root search directory.  Allowed values are ``ask`` for user
     prompt, ``current`` for current dir and ``project`` for project
     root.  Any form that evaluates to a directory string is also allowed.
     Default is ``ask``.
   
   - **:confirm** - ``never``, ``always``, or ``prefix`` are allowed values.  Specifies
     if the the final search command line string can be modified
     and confirmed the user. Default is ``never``.
   
   - **:flags** - ``ask`` or a list of command line flags that will be used when
     invoking the search.
   
   - **:menu** - Bind the command into ``rg-menu``.  Must be a list with three
     items in it.  The first item is the description of the
     group in which the new command will appear.  If the group
     does not exist a new will be created.  The second item is
     the key binding for this new command (ether a key vector
     or a key description string) and the third item is the
     description of the command that will appear in the menu.
   
   Examples:
   
   .. code-block:: elisp
   
       (rg-define-search search-everything-at-home
         "Search files including hidden in home directory"
         :query ask
         :format literal
         :files "everything"
         :flags ("--hidden")
         :dir (getenv "HOME")
         :menu ("Search" "h" "Home"))
   
       (rg-define-search rg-emacs
         "Search the emacs lisp source code."
         :dir "/usr/share/emacs/25.2/lisp/"
         :flags '("-z")
         :files "*.{el,el.gz}"
         :menu ("Custom" "L" "lisp"))

Use with evil-mode
------------------

Some key bindings clash with *evil-mode*. Recommendation is to use
evil *motion* state for the results buffer and then switch to
evil *normal* mode when editing in *wgrep-mode*. Some adjustments
need to be done to avoid the clashes though.

This is a start of a configuration. This let *rg-mode*'s key bindings
override the motion state map bindings based on that these motion
keys are not important in an *rg* results buffer.
Adjust this to your preferred use case:

.. code-block:: elisp

    (with-eval-after-load 'rg
      (advice-add 'wgrep-change-to-wgrep-mode :after
    	      #'evil-normal-state)
      (advice-add 'wgrep-to-original-mode :after
    	      #'evil-motion-state)
      (defvar rg-mode-map)
      (add-to-list 'evil-motion-state-modes 'rg-mode)
      (evil-add-hjkl-bindings rg-mode-map 'motion
        "e" #'wgrep-change-to-wgrep-mode
        "g" #'rg-recompile
        "t" #'rg-rerun-change-literal))

.. _customizing_the_menu:

Customizing the menu
--------------------

The menu can be modified from the emacs configuration file.

To add a new **switch** before the option triggered by ``-n`` at suffix
level 3:

.. code-block:: elisp

    (transient-insert-suffix 'rg-menu "-n" '(3 "-o" "Only print matches" "--only-matching"))

To add a new **option** before the option triggered by ``-g`` at suffix
level 4:

.. code-block:: elisp

    (transient-insert-suffix 'rg-menu "-g" '(4 "-f" "Pattern file" "--file="))

The ``=`` in ``--file=`` triggers argument input for the flag.

To remove an item from the menu specify the trigger key in the
transient remove command.
For example, to remove the ``Search hidden files`` switch use the following:

.. code-block:: elisp

    (transient-remove-suffix 'rg-menu "-h")

Please refer to the `transient <https://magit.vc/manual/transient/Modifying-Existing-Transients.html#Modifying-Existing-Transients>`_ documentation for details on customizing the menu.

This package also adds a convenience function for appending new
**commands** to the menu in the groups at the bottom.

.. function:: (rg-menu-transient-insert group key description command)
   
   This inserts a new command under ``group`` if it exists, otherwise a
   new group is created. ``key``, ``description`` and ``command`` is as for
   the ``transient-insert-suffix`` function.
   
   For example to insert a new command under ``Search`` group:
   
   .. code-block:: elisp
   
       (rg-menu-transient-insert "Search" "m" "My search" 'my-search-command)
   
   It's usually better to use the ``:menu`` key of the :func:`rg-define-search`
   macro to define a search function and adding it to the menu in one go.
