# Transmute

Common Lisp Deatabase Independent Migration tool.

## Usage

```lisp

;;Run migrations.
(migrate (:mysql
          :database-name "test"
          :username "nobody"
          :password "1234"))

;;Create migration.
(gen-migration "create-users")
```

## Installation

## API

- \*migration-dir\*
- migration [db-spec]
- gen-new-migration [migration-name]

## TODO

- Migration template.
- Warning
- Clack middleware.

## Author

* Masato Sogame (poketo7878@gmail.com)

## Copyright

Copyright (c) 2015 Masato Sogame (poketo7878@gmail.com)

## License

Licensed under the LLGPL License.
