package main

import (
	"fmt"
	"os"
	"os/exec"

	"github.com/polydawn/meep"
	"github.com/tazjin/kontemplate/context"
	"github.com/tazjin/kontemplate/templater"
	"gopkg.in/alecthomas/kingpin.v2"
)

const version string = "1.0"
// This variable will be initialised by the Go linker during the builder
var gitHash string

type KubeCtlError struct {
	meep.AllTraits
}

var (
	app = kingpin.New("kontemplate", "simple Kubernetes resource templating")

	// Global flags
	includes = app.Flag("include", "Resource sets to include explicitly").Short('i').Strings()
	excludes = app.Flag("exclude", "Resource sets to exclude explicitly").Short('e').Strings()

	// Commands
	template     = app.Command("template", "Template resource sets and print them")
	templateFile = template.Arg("file", "Cluster configuration file to use").Required().String()

	apply       = app.Command("apply", "Template resources and pass to 'kubectl apply'")
	applyFile   = apply.Arg("file", "Cluster configuration file to use").Required().String()
	applyDryRun = apply.Flag("dry-run", "Print remote operations without executing them").Default("false").Bool()

	replace     = app.Command("replace", "Template resources and pass to 'kubectl replace'")
	replaceFile = replace.Arg("file", "Cluster configuration file to use").Required().String()

	delete     = app.Command("delete", "Template resources and pass to 'kubectl delete'")
	deleteFile = delete.Arg("file", "Cluster configuration file to use").Required().String()

	create     = app.Command("create", "Template resources and pass to 'kubectl create'")
	createFile = create.Arg("file", "Cluster configuration file to use").Required().String()

	versionCmd = app.Command("version", "Show kontemplate version")
)

func main() {
	app.HelpFlag.Short('h')

	switch kingpin.MustParse(app.Parse(os.Args[1:])) {
	case template.FullCommand():
		templateCommand()

	case apply.FullCommand():
		applyCommand()

	case replace.FullCommand():
		replaceCommand()

	case delete.FullCommand():
		deleteCommand()

	case create.FullCommand():
		createCommand()

	case versionCmd.FullCommand():
		versionCommand()
	}
}

func versionCommand() {
	if gitHash == "" {
		fmt.Printf("Kontemplate version %s (git commit unknown)\n", version)
	} else {
		fmt.Printf("Kontemplate version %s (git commit: %s)\n", version, gitHash)
	}
}

func templateCommand() {
	_, resources := loadContextAndResources(templateFile)

	for _, r := range *resources {
		fmt.Println(r)
	}
}

func applyCommand() {
	ctx, resources := loadContextAndResources(applyFile)

	var kubectlArgs []string

	if *applyDryRun {
		kubectlArgs = []string{"apply", "-f", "-", "--dry-run"}
	} else {
		kubectlArgs = []string{"apply", "-f", "-"}
	}

	runKubectlWithResources(ctx, &kubectlArgs, resources)
}

func replaceCommand() {
	ctx, resources := loadContextAndResources(replaceFile)
	args := []string{"replace", "--save-config=true", "-f", "-"}
	runKubectlWithResources(ctx, &args, resources)
}

func deleteCommand() {
	ctx, resources := loadContextAndResources(deleteFile)
	args := []string{"delete", "-f", "-"}
	runKubectlWithResources(ctx, &args, resources)
}

func createCommand() {
	ctx, resources := loadContextAndResources(createFile)
	args := []string{"create", "--save-config=true", "-f", "-"}
	runKubectlWithResources(ctx, &args, resources)
}

func loadContextAndResources(file *string) (*context.Context, *[]string) {
	ctx, err := context.LoadContextFromFile(*file)
	if err != nil {
		app.Fatalf("Error loading context: %v\n", err)
	}

	resources, err := templater.LoadAndPrepareTemplates(includes, excludes, ctx)
	if err != nil {
		app.Fatalf("Error templating resource sets: %v\n", err)
	}

	return ctx, &resources
}

func runKubectlWithResources(c *context.Context, kubectlArgs *[]string, resources *[]string) error {
	args := append(*kubectlArgs, fmt.Sprintf("--context=%s", c.Name))

	kubectl := exec.Command("kubectl", args...)

	stdin, err := kubectl.StdinPipe()
	if err != nil {
		return meep.New(&KubeCtlError{}, meep.Cause(err))
	}

	kubectl.Stdout = os.Stdout
	kubectl.Stderr = os.Stderr

	if err = kubectl.Start(); err != nil {
		return meep.New(&KubeCtlError{}, meep.Cause(err))
	}

	for _, r := range *resources {
		fmt.Fprintln(stdin, r)
	}
	stdin.Close()

	kubectl.Wait()

	return nil
}
