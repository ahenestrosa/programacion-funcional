module Web.View.Messages.New where
import Web.View.Prelude

data NewView = NewView {}

instance View NewView where
    html NewView { } = [hsx|
        {breadcrumb}
        <h1>New Message</h1>


        <form method="POST" action="/CreateMessage" id="" class="new-form">
            <div class="form-group" id="form-group-message_text">
                <label for="message_text">Text</label>
                <input type="text" name="text" id="message_text" class="form-control" />
            </div>

            <div class="form-group" id="form-group-message_file">
                <input    
                    type="file"
                    name="markdown"
                    accept="application/pdf"
                >
            </div>


            <button class="btn btn-primary">Create Message</button>
        </form>
        <!-- {renderForm message} -->

    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbText "New Message"
                ]

-- renderForm :: Message -> Html
-- renderForm message = formFor message [hsx|
--     {(textField #text)}
--     {submitButton}

-- |]