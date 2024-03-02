package config

import (
	"os"
	"text/template"
)

const Template string = `
# -*- makefile -*-
# name: {{.Name}}
# version: {{.Version}}
{{- range .Headers }}
{{ . -}}
{{- end }}

# core
{{ range $_, $key := .Core}}
{{ $key }} {{ join (index $.Exts $key) " " -}}
{{- end }}

{{ if .Static}} 
*static* 
{{ else }} 
{{ end }}
{{ range $_, $key := .Static}}
{{ $key }} {{ join (index $.Exts $key) " " -}}
{{- end }}

{{ if .Shared}}
*shared*
{{ else }} 
{{ end }}
{{ range $_, $key := .Shared}}
{{ $key }} {{ join (index $.Exts $key) " " -}}
{{- end }}

*disabled*
{{ range $key, $value := .Disabled}}
{{ $value -}}
{{- end }}

# end
`

func createFileUsingTemplate(t *template.Template, filename string, data interface{}) error {
	f, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer f.Close()

	err = t.Execute(f, data)
	if err != nil {
		return err
	}

	return nil
}
