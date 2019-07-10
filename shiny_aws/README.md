# Setting up R Shiny on AWS with Auth0  

R Shiny is a great way to expand your R code into interactive applications. However, when it comes to sharing that app with others (i.e. - the public, your coworkers, a client), you need to think about how to **deploy the app** (move it from your computer the to web), and **authenticate it** (restrict access to specific users).  

RStudio offers [Shiny Server Pro](https://www.rstudio.com/products/shiny-server-pro/) with a high-level user dashboard that allows for one-click publishing of Shiny apps, adding a custom domain, and server load balancing among other features. However, for smaller projects, the open-source [Shiny Server](https://www.rstudio.com/products/shiny/download-server/) hosted on a cloud server (e.g - AWS, Google Cloud, Digital Ocean) is sufficient and can be much more affordable. In this blog post, we cover how to:  

1. Set up an AWS account  
2. Install R and Shiny Server  
3. Add a custom domain name with nginx  
4. Add authorization with Auth0  
5. Configure Auth0 to restrict access to specific users, email domains, etc
6. Load balance with ???  

***  

Here’s a list of blog posts on aspects of this blog post that informed our path forward.  

1. [Linux/Windows])https://www.charlesbordet.com/en/shiny-aws-3/#how-to-install-shiny-server)  
2. [Windows](https://towardsdatascience.com/how-to-host-a-r-shiny-app-on-aws-cloud-in-7-simple-steps-5595e7885722)  
3. [Linux](https://tm3.ghost.io/2017/12/31/deploying-an-r-shiny-app-to-aws/)  
4. [Auth0](https://auth0.com/blog/adding-authentication-to-shiny-server/)  

***  

# Step 1: Set up an AWS account  

Create an [AWS account](https://aws.amazon.com/), and start an EC2 instance.   

From the choices of Amazon Machine Images (AMI) select the **Ubuntu Server 18.04 LTS**.  

Select instance type as `t2.micro` (1GB RAM). You can always change it later from the user dashboard, and may need to upgrade it to install heavier packages (for example, dplyr is too heavy to install on a t2.micro).  

Select the default 8GB storage.  

Don't worry about adding a tag.  

Configure Security settings. This is where we specifcy what can connect to our instance and via what methods. Add:  

| Type          | Protocol      | Port Range    | Source        | Description   |  
| ------------- | ------------- | ------------- | ------------- | ------------- |  
| SSH           | TCP           | 22            | Anywhere      | remote login  |  
| HTTP          | TCP           | 80            | Anywhere      | web access    |  
| Custom TCP    | TCP           | 3838          | Anywhere      | default Shiny |  

More security features may need to be added later, depending on what else needs access to the instance. Http is also okay for dev, but will want https in the final product.  

Create a key pair. Name it `key.pem`, and store it somewhere easily accessible like `~/Desktop/aws`.  

Protect the file from overwritewith `chmod`, otherwise AWS will throw the error: `“WARNING: UNPROTECTED PRIVATE KEY FILE!`  

`chmod 400 ~/Desktop/aws/key.pem`   

Now it's time to connect to your instance. In the AWS Dashboard, click on "Instances" in the left hand toolbar. Make sure your instance is running, and click on the "Connect" button to the right of the blue "Launch Instace" button. Copy the connection information. It will look something like:  

`ssh -i "key.pem" ubuntu@ec2-my_public_IP_address.us-east-2.compute.amazonaws.com`  

If using Linux-based OS, open a terminal Change directories into where your key file is, and paste this ssh command. If using Windows, use PuTTY.  

If you're unable to connect, reboot the instance, and restart your computer before troubleshooting yourself down a rabbit hole.  


***  


# Step 2: Install R and Shiny Server  

```
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9  
sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
sudo apt update  
sudo apt install r-base 
sudo R  
install.packages("shiny")  # install shiny
quit()                     # quit R
n                          # don't save workspace
```

Now we download the [latest version of Shiny Server](https://www.rstudio.com/products/shiny/download-server/) from RStudio. It should be those last 3 commands on the link in the previous sentence. For example, at the time of writing, mine is:  

``` 
sudo apt-get install gdebi-core  
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.9.923-amd64.deb  
sudo gdebi shiny-server-1.5.9.923-amd64.deb  
```

View the example app that comes with vanilla shiny server in a browser via HTTP. Try: IP_address_of_instance:3838. Port 3838 is the defauly Shiny Server port we opened in our security settings, and the IP address of the EC2 instance can be found on the AWS dashboard.  

If it works, you should see a page entitled "Welcome to Shiny Server!".  

Shiny apps are stored in `/srv/shiny-server`. If you `ls /srv/shiny-server` you'll see `index.html` and `sample-apps`. The `index.html` is what you seee when you go to IP_address_of_instance:3838.  

To add a shiny app to serve, `git clone` an app ([here are some examples](https://github.com/rstudio/shiny-examples)) into your home directory, and symlink it into `/srv/shiny-server`:  

```
cd  
git clone <URL of app to clone from github>  
cd /srv/shiny-server
sudo ln -s ~/name_of_folder_with_app  
```

To view this new app, you'll aso need to remove the index file: `sudo rm /srv/shiny-server/index.html`  

The shiny server configuration file is at `/etc/shiny-server/shiny-server.conf`.  

Edit it with `sudo nano /etc/shiny-server/shiny-server.conf`. You'll want to add to the top line `preserve_logs true;` to start logging errors in `/var/log/shiny-server`. You'll also want to change `directory_index on;` to `directory_index off;` to remove the directory index screen you encounter when visiting the page.  

To make these changes to `shiny-server.conf` live, restart shiny server: `sudo systemctl reload shiny-server`  

By the way, when installing additional R packages, do so from the user `shiny`, like so:    

```
sudo su - shiny  
R  
install.packages("ggplot2") # and say "yes" to the local library  
```

Type `exit` in the console to get back to user `ubuntu`.  


A few tips related to installing R packages:  

* The _t2.micro_ is too small to install some heavier `R` packages like `dplyr` . Switch to a _t2.large_ instance for this. It's only $0.09 per hour.  
* For many core R libraries, you'll need to `sudo apt install libssl-dev` as user `ubuntu` .


***  

# Step 3: Add a custom domain name with nginx  

See [this beginner's guide](http://nginx.org/en/docs/beginners_guide.html).  

Currently all requests to `richpauloo.com` and all subdomains `*.richpauloo.com` redirect to some IP. We want all requests to `shiny.richpauloo.com` to direct to the IP address of the AWS server.  

To do this, go to your DNS registrar (e.g. - GoDaddy) and manage the domain you select (e.g. `richpauloo.com`). Remove the present `TYPE` _A_ records. Then make a new `TYPE` _A_, with `HOST` _shiny_, and `VALUE` equal to the IP address of your EC2 instance. Now _shiny_ is a subdomain of `richpauloo.com` , so requests to `shiny.richpauloo` are redirected to the IP address provided, which is the EC2 instance!  

Next we install `nginx` for the reverse name proxy: a user requests `shiny.richpauloo.com`, and talks instead to `nginx`, which we configure to redirect to the app!  

Here's a [good GH](https://serverfault.com/questions/527630/what-is-the-different-usages-for-sites-available-vs-the-conf-d-directory-for-ngi) about the difference between `nginx.conf` and the  `sites-enabled/sites-available` config methods...  

I added this configuration file: `sudo nano /etc/nginx/sites-available/shiny.conf` :  

```
server {
    # listen 80 means the Nginx server listens on the 80 port.
    listen 80;
    listen [::]:80;
    # Replace it with your (sub)domain name.
    server_name shiny.richpauloo.com;
    # The reverse proxy, keep this unchanged:
    location / {
        proxy_pass http://localhost:3838;
        proxy_redirect http://localhost:3838/ $scheme://$host/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;
        proxy_read_timeout 20d;
        proxy_buffering off;
    }
}
```

Then symlinked this file:  

```
cd /etc/nginx/sites-enabled  
sudo ln -s /etc/niginx/sites-available/shiny.conf
```

The reasoning behind this linking is that you can simply unlink the file when it's not necessary, but retain it in `sites-available`.  

Test the `.conf` with `sudo nginx -t`.  

To see the changes `sudo systemctl restart nginx` . You might also need to restart shiny server with `sudo systemctl reload shiny server` .  

Now the app should render at `shiny.richpauloo.com/my_app` .  



***  


# THIS IS WHERE I LEFT OFF - ANYTHIGN BELOW IS SUSPECT  

***  



# Step 4: Add authorization with Auth0  
# Step 5: Configure Auth0 to restrict access to specific users, email domains, etc
# Step 6: Load balance with ???  



# Auth0 following source 4


need domain, client name and secret to setup shiny-auth0  

- domain: dev-hfd8x0kx.auth0.com
- client id: WsmsAMZARLrGrmQb7O8Sc6rJE9h0a7VR
- client secret: tdzSP9M_cp6tYjn9flumXUOu2z-ceNDunxd8LbFS-dg6m3yeylXktm6H_rIOGp3w


.env file

AUTH0_CLIENT_SECRET=tdzSP9M_cp6tYjn9flumXUOu2z-ceNDunxd8LbFS-dg6m3yeylXktm6H_rIOGp3w
AUTH0_CLIENT_ID=WsmsAMZARLrGrmQb7O8Sc6rJE9h0a7VR
AUTH0_DOMAIN=dev-hfd8x0kx.auth0.com
AUTH0_CALLBACK_URL=http://shiny.richpauloo.com/movie-explorer
COOKIE_SECRET=kjasdhf098342lekfjfKJHkjhoiu56
SHINY_HOST=localhost
SHINY_PORT=3838
PORT=3000





***  

/etc/shiny-server/shiny-server.conf

# save error logs
preserve_logs true;

# Instruct Shiny Server to run applications as the user "shiny"
run_as shiny;

# Define a server that listens on port 3838
server {
  listen 3838;

  # Define a location at the base URL
  location / {

    # Host the directory of Shiny Apps stored in this directory
    site_dir /srv/shiny-server;

    # Log all Shiny output to files in this directory
    log_dir /var/log/shiny-server;

    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index off;
  }
}


***  


Godaddy bought richpauloo.com

set A record as

host: shiny
points to: AWS EC2 IP

Then followed source 1 to install nginx 

In cd /etc/nginx/sites-available/ the shiny.conf file we make needs change line
`$connection_upgrade` to just `upgrade`  for `sudo nginx -t` test to work

then restart nginx: `sudo systemctl restart nginx`  

now my app is on a fancy IP: http://shiny.richpauloo.com/movie-explorer/



git clone https:/...shiny-auth0


sudo nano .env

AUTH0_CLIENT_SECRET=tdzSP9M_cp6tYjn9flumXUOu2z-ceNDunxd8LbFS-dg6m3yeylXktm6H_rIOGp3w
AUTH0_CLIENT_ID=WsmsAMZARLrGrmQb7O8Sc6rJE9h0a7VR
AUTH0_DOMAIN=dev-hfd8x0kx.auth0.com
AUTH0_CALLBACK_URL=http://shiny.richpauloo.com/movie-explorer
COOKIE_SECRET=UMvMXpvRQR5fYU26pskv
SHINY_HOST=localhost
SHINY_PORT=3838
PORT=3000


# system wide ngnix conf file

/etc/nginx/nginx.conf



events {
}

http {
    map $http_upgrade $connection_upgrade {
        default upgrade;
        ''      close;
    }

    # Listen on port 80 and redirect all requests to the
    # TLS enabled server (https, port 443)
    server {
        listen       *:80;

        # Your hostname should go here
        server_name  shiny.richpauloo.com;

        access_log   off;
        location / {
            rewrite ^ https://$host$request_uri? permanent;
        }
    }

    # TLS enabled server
    server {
        listen       443 ssl;

        # Your hostname should go here
        server_name shiny.richpauloo.com;

        # TLS/SSL certificates for your secure server should go here.
        # If you don't have a TLS certificate, you can get a free one by
        # following the free PDF available in this link:
        # https://auth0.com/blog/using-https/
        # ssl_certificate      localtestserver-dot-com.pem;
        # ssl_certificate_key  localtestserver-dot-com-key.pem;

        # To enhance security, as long as you don't need to support older browsers
        # (and you probably don't), you should only enable the most secure
        # ciphers and algorithms. This is a sane selection.
        ssl_ciphers 'ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:DHE-RSA-AES256-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128:AES256:AES:DES-CBC3-SHA:HIGH:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK';
        ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
        ssl_prefer_server_ciphers on;
        ssl_session_cache  builtin:1000  shared:SSL:10m;
        ssl_stapling on; # Requires nginx >= 1.3.7
        ssl_stapling_verify on; # Requires nginx => 1.3.7

        # This proxies requests to our shiny-auth0 authentication proxy.
        # Requests are passed in plain HTTP, so TLS termination
        # is applied at this point.
        location / {
            proxy_set_header    Host $host;

            # This points to our shiny-auth0 authentication proxy,
            # change localhost:3000 to suit the configuration of
            # your shiny-auth0 config
            proxy_pass          http://localhost:3000;
            proxy_redirect      http://localhost:3000/ $scheme://$host/;

            proxy_http_version  1.1;

            # The following lines enable WebSockets proxying, do not remove them
            # as they are used by Shiny Server to improve user experience
            proxy_set_header    Upgrade $http_upgrade;
            proxy_set_header    Connection $connection_upgrade;

            proxy_connect_timeout 3h;
            proxy_send_timeout 3h;
            proxy_read_timeout 3h;
        }
    }
}



***  


# default nginx.conf file


# User and group used by worker processes
user www-data;

# Ideally # of worker processes = # of CPUs or cores
# Set to auto to autodetect
# max_clients = worker_processes * worker_connections
worker_processes auto;

pid /run/nginx.pid;

include /etc/nginx/modeules-enabled/*.conf;  

# Maximum number of open file descriptors per process
# should be > worker_connections
# worker_rlimit_nofile 10240;

events {
    # Use epoll on Linux 2.6+
    use epoll;
    # Max number of simultaneous connections per worker process
    worker_connections 768;
    # Accept all new connections at one time
    # multi_accept on;
}

http {

    ##
    # Basic Settings
    ##

    # Hide nginx version information
    server_tokens off;

    # Speed up file transfers by using sendfile() to copy directly
    # between descriptors rather than using read()/write()
    sendfile on;

    # Tell Nginx not to send out partial frames; this increases throughput
    # since TCP frames are filled up before being sent out (adds TCP_CORK)
    # Send the response header and the beginning of a file in one packet
    # Send a file in full packets
    tcp_nopush on;

    # Tell Nginx to enable the Nagle buffering algorithm for TCP packets
    # which collates several smaller packets together into one larger packet
    # thus saving bandwidth at the cost of a nearly imperceptible increase to latency
    tcp_nodelay on;
    
    # send_timeout 30;

    # How long to allow each connection to stay idle;
    # Longer values are better for each individual client, especially SSL
    # But means that worker connections are tied up longer.75
    keepalive_timeout 65;

    # keepalive_requests 200;
    # client_header_timeout 20;
    # client_body_timeout 20;
    # reset_timedout_connection on;
    
    types_hash_max_size 2048;

    # server_names_hash_bucket_size 64;
    # server_name_in_redirect off;

    include /etc/nginx/mime.types;
    default_type application/octet-stream;
    # default_type text/html;
    # charset UTF-8;

    ##
    # SSL Settings
    ##

    ssl_protocols TLSv1 TLSv1.1 TLSv1.2; # Dropping SSLv3, ref: POODLE  
    ssl_prefer_server_ciphers shion;

    ##
    # Logging Settings
    ##

    access_log /var/log/nginx/access.log;
    error_log /var/log/nginx/error.log;

    ##
    # Gzip Settings
    ##

    # Enable Gzip compression
    gzip on;

    # This should be turned on if pre-compressed copies (.gz) of static files exist
    # If NOT it should be left off as it will cause extra I/O
    # default: off
    # gzip_static on;

    # Do NOT compress anything smaller than 256 bytes
    # gzip_min_length 256;

    # Fuck IE6
    # gzip_disable "msie6";

    # Tell proxies to cache both the gzipped and regular version of a resource
    # whenever the client's Accept-Encoding capabilities header varies;
    # Avoids the issue where a non-gzip capable client (rare)
    # would display gibberish if their proxy gave them the gzipped version.
    # gzip_vary on;

    # Compress data even for clients that are connecting via proxies
    # Identified by the "Via" header
    # gzip_proxied any;

    # Compression level (1-9)
    # 5 is the perfect compromise between size and CPU usage
    # gzip_comp_level 5;
    
    # gzip_buffers 16 8k;
    # gzip_http_version 1.1;


    # Cache open file descriptors, their sizes and mtime
    # information on existence of directories
    # file lookup error such as "file not found", "no read permission" and so on
    #
    # Pros: nginx can immediately begin sending data when a popular file is requested
    # and will also immediately send a 404 if a file doesn't exist, and so on
    #
    # Cons: The server will NOT react immediately to changes on file system
    # which may be undesirable
    #
    # Config: inactive files are released from the cache after 20 seconds
    # whereas active (recently requested) files are re-validated every 30 seconds
    # File descriptors will NOT be cached unless they are used at least twice in 20s (inactive)
    #
    # A maximum of the 1000 most recently used file descriptors will be cached at any time
    #
    # Production servers with stable file collections will definitely want to enable the cache
    # open_file_cache max=1000 inactive=20s;
    # open_file_cache_valid    30s;
    # open_file_cache_min_uses 2;
    # open_file_cache_errors   on;

    ##
    # nginx-naxsi config
    ##
    # Uncomment it if you installed nginx-naxsi
    ##

    #include /etc/nginx/naxsi_core.rules;

    ##
    # nginx-passenger config
    ##
    # Uncomment it if you installed nginx-passenger
    ##
    
    #passenger_root /usr;
    #passenger_ruby /usr/bin/ruby;

    ##
    # Virtual Host Configs
    ##

    include /etc/nginx/conf.d/*.conf;
    include /etc/nginx/sites-enabled/*;
}


#mail {
#   # See sample authentication script at:
#   # http://wiki.nginx.org/ImapAuthenticateWithApachePhpScript
# 
#   # auth_http localhost/auth.php;
#   # pop3_capabilities "TOP" "USER";
#   # imap_capabilities "IMAP4rev1" "UIDPLUS";
# 
#   server {
#       listen     localhost:110;
#       protocol   pop3;
#       proxy      on;
#   }
# 
#   server {
#       listen     localhost:143;
#       protocol   imap;
#       proxy      on;
#   }
#}








