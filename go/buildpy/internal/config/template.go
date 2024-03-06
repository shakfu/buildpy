package config

const Template string = `# -*- makefile -*-
# name: {{.Name}}
# version: {{.Version}}

# headers
{{ range .Headers }}
{{ . -}}
{{- end }}

# core

{{- range $_, $key := .Core}}
{{ $key }} {{ join (index $.Exts $key) " " -}}
{{- end }}

{{ if .Static}} 
*static* 
{{ else }} 
{{ end }}
{{ range $key, $value := .Static}}
{{ $key }} {{ join (index $.Exts $key) " " -}}
{{- end }}

{{ if .Shared}}
*shared*
{{ else }} 
{{ end }}
{{ range $key, $value := .Shared}}
{{ $key }} {{ join (index $.Exts $key) " " -}}
{{- end }}

*disabled*

{{- range $key, $value := .Disabled}}
{{ $key -}}
{{- end }}

# end
`
