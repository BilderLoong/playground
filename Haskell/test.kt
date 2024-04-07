import java.net.URI

fun main() {
    val uriString = "https://example.com/path?param=value"

    val uri = URI(uriString)

    // Accessing URI components
    val scheme = uri.scheme
    val host = uri.host
    val path = uri.path
    val query = uri.query

    println("Scheme: $scheme")
    println("Host: $host")
    println("Path: $path")
    println("Query: $query")

    // Manipulating URI components
    val newScheme = "http"
    val newPath = "/new-path"
    val newQuery = "url=/people-count/index?tenantId=10036586&poiId=600296491"

    val updatedUri = URI(newScheme, uri.userInfo, uri.host, uri.port, newPath, newQuery, uri.fragment)

    println("Updated URI: $updatedUri")
}
