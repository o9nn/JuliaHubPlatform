#!/usr/bin/env julia

"""
Example usage of the JuliaHub Server-Side Platform

This script demonstrates how to use all the major features of the server-side platform.
"""

using JuliaHub

# Start the server
println("Starting JuliaHub Server-Side Platform...")
config = JuliaHub.Server.ServerConfig(
    host="0.0.0.0",
    port=8080,
    max_workers=10,
    storage_path="./demo_storage"
)
state = JuliaHub.Server.start_server(config)
println("✓ Server started on port $(config.port)")

# 1. Create coding environments
println("\n1. Creating coding environments...")
pluto_env = JuliaHub.Server.create_coding_environment(
    :pluto, "alice", "/notebooks/analysis.jl", port=1234
)
println("✓ Created Pluto environment: $(pluto_env.id)")

ide_env = JuliaHub.Server.create_coding_environment(
    :julia_ide, "bob", "/workspace", port=8081
)
println("✓ Created JuliaIDE environment: $(ide_env.id)")

# 2. Create a team project
println("\n2. Creating team project...")
project = JuliaHub.Server.create_team_project(
    "Data Analysis Project",
    "A collaborative data analysis project",
    "alice"
)
println("✓ Created project: $(project.project.name)")

# Add team member
JuliaHub.Server.add_project_member(project.project.id, "bob", ["read", "write"])
println("✓ Added bob as team member")

# Share files
JuliaHub.Server.share_project_file(project.project.id, "/data/results.csv")
println("✓ Shared file: /data/results.csv")

# 3. Create Time Capsule snapshot
println("\n3. Creating Time Capsule snapshot...")
capsule = JuliaHub.Server.create_snapshot(
    "production-env-2025",
    "alice",
    "Production environment snapshot for reproducibility"
)
println("✓ Created Time Capsule: $(capsule.name)")

# 4. Set up CloudStation HPC
println("\n4. Setting up CloudStation HPC...")
station = JuliaHub.Server.create_cloudstation("Main HPC Cluster")
println("✓ Created CloudStation: $(station.name)")

# Add HPC nodes
node1 = JuliaHub.Server.add_hpc_node(station.id, "compute-1", 64, 256, 8)
node2 = JuliaHub.Server.add_hpc_node(station.id, "compute-2", 64, 256, 8)
println("✓ Added $(length(station.nodes)) HPC nodes")

# Submit HPC job
job_id = JuliaHub.Server.submit_hpc_job(
    station.id,
    "monte-carlo-simulation",
    """
    using Distributed
    @everywhere function monte_carlo_pi(n)
        count = 0
        for i in 1:n
            x, y = rand(), rand()
            if x^2 + y^2 <= 1
                count += 1
            end
        end
        return 4 * count / n
    end
    
    result = mean(pmap(monte_carlo_pi, fill(10^6, 100)))
    println("Estimated π: ", result)
    """,
    cores=32,
    memory_gb=128
)
println("✓ Submitted HPC job: $job_id")

# 5. Package registry
println("\n5. Managing package registry...")
registry = JuliaHub.Server.create_package_registry(
    "MyOrgRegistry",
    "https://github.com/myorg/registry"
)
println("✓ Created registry: $(registry.name)")

JuliaHub.Server.register_package(
    registry.id,
    "DataAnalysisTools",
    "1.0.0",
    "daf8e5c3-1234-5678-90ab-cdef12345678",
    Dict("DataFrames" => "1.3", "Plots" => "1.29")
)
println("✓ Registered package: DataAnalysisTools v1.0.0")

# 6. Deploy dashboard application
println("\n6. Deploying dashboard application...")
dashboard = JuliaHub.Server.deploy_dashboard(
    "Analytics Dashboard",
    "alice",
    "Genie",
    port=8002
)
println("✓ Deployed dashboard at: $(dashboard.url)")

# 7. API endpoints and notifications
println("\n7. Setting up APIs and notifications...")
endpoint = JuliaHub.Server.create_api_endpoint(
    "/api/v1/data/query",
    "POST",
    "Data query endpoint",
    auth_required=true,
    rate_limit=1000
)
println("✓ Created API endpoint: $(endpoint.path)")

notification_service = JuliaHub.Server.create_notification_service(
    "MainNotificationService",
    channels=["email", "webhook", "slack"]
)
println("✓ Created notification service")

JuliaHub.Server.send_notification(
    notification_service.id,
    "alice@example.com",
    "Your HPC job has been queued",
    "email",
    priority="normal"
)
println("✓ Sent notification to alice")

# 8. Code analysis
println("\n8. Running static code analysis...")
analyzer = JuliaHub.Server.create_code_analyzer("SecurityAnalyzer")
result = JuliaHub.Server.run_static_analysis(analyzer, "/src/main.jl")
summary = JuliaHub.Server.get_analysis_summary(result.id)
println("✓ Analysis complete: $(summary["total_issues"]) issues found")

# 9. Traceability and compliance
println("\n9. Setting up traceability and compliance...")
using Dates

JuliaHub.Server.log_operation(
    "alice",
    "create",
    "project",
    project.project.id,
    details=Dict{String, Any}("name" => project.project.name)
)
println("✓ Logged operation")

report = JuliaHub.Server.generate_compliance_report(
    "December 2025 Report",
    DateTime(2025, 12, 1),
    DateTime(2025, 12, 31)
)
println("✓ Generated compliance report: $(report.summary["total_operations"]) operations")

# 10. ChatGPT service
println("\n10. Setting up ChatGPT service...")
chatgpt = JuliaHub.Server.create_chatgpt_service("AI Assistant")
response = JuliaHub.Server.query_chatgpt(
    chatgpt.id,
    "alice",
    "How can I optimize my Julia code for performance?"
)
println("✓ ChatGPT response: $(first(response["response"], 80))...")

# 11. Quarto reports
println("\n11. Creating Quarto report...")
quarto_report = JuliaHub.Server.create_quarto_report(
    "Monthly Analysis Report",
    "alice",
    "/reports/monthly.qmd",
    format="html"
)
rendered = JuliaHub.Server.render_quarto(quarto_report.id, execute=true)
println("✓ Rendered Quarto report: $(rendered.name)")

# 12. Integrations
println("\n12. Setting up integrations...")
rstudio = JuliaHub.Server.create_rstudio_integration(
    "RStudio Server",
    "/rstudio-workspace",
    r_version="4.3.0",
    port=8787
)
println("✓ Created RStudio integration")

gitlens = JuliaHub.Server.create_gitlens_integration(
    "Project Repository",
    "https://github.com/myorg/data-analysis"
)
println("✓ Created GitLens integration")

windows = JuliaHub.Server.create_windows_workstation_integration(
    "Windows Dev Station",
    "dev-ws-01.example.com",
    port=3389,
    gpu_support=true
)
println("✓ Created Windows Workstation integration")

# Summary
println("\n" * "="^60)
println("JuliaHub Server-Side Platform Demo Complete!")
println("="^60)
println("Active Resources:")
println("  - Coding Environments: $(length(state["coding_environments"]))")
println("  - Team Projects: $(length(state["projects"]))")
println("  - Time Capsules: $(length(state["time_capsules"]))")
println("  - CloudStation Nodes: $(length(station.nodes))")
println("  - Package Registries: $(length(state["package_registries"]))")
println("  - Dashboard Apps: $(length(state["dashboard_apps"]))")
println("  - API Endpoints: $(length(state["api_endpoints"]))")
println("  - Integrations: $(length(state["integrations"]))")
println("\nServer is running on http://$(config.host):$(config.port)")
println("Press Ctrl+C to stop the server")

# Keep the server running
try
    while true
        sleep(1)
    end
catch e
    if e isa InterruptException
        println("\n\nShutting down...")
        JuliaHub.Server.stop_server()
        println("✓ Server stopped")
    else
        rethrow(e)
    end
end
